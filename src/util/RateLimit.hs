-- AUTHOR: Adam Wick <awick@uhsure.com>
-- PACKAGE: https://hackage.haskell.org/package/rate-limit

-- | This module implements rate-limiting functionality for Haskell programs.
-- Rate-limiting is useful when trying to control / limit access to a
-- particular resource over time. For example, you might want to limit the
-- rate at which you make requests to a server, as an administrator may block
-- your access if you make too many requests too quickly. Similarly, one may
-- wish to rate-limit certain communication actions, in order to avoid
-- accidentally performing a denial-of-service attack on a critical resource.
--
-- The fundamental idea of this library is that given some basic information
-- about the requests you wante rate limited, it will return you a function
-- that hides all the rate-limiting detail. In short, you make a call to one
-- of the function generators in this file, and you will be returned a function
-- to use. For example:
--
-- @
--   do f <- generateRateLimitedFunction ...
--      ...
--      res1 <- f a
--      ...
--      res2 <- f b
--      ...
-- @
--
-- The calls to the generated function (f) will be rate limited based on the
-- parameters given to 'generateRateLimitedFunction'.
--
-- 'generateRateLimitedFunction' is the most general version of the rate
-- limiting functionality, but specialized versions of it are also exported
-- for convenience.

-- (Benjamin Bray / 2021-05-25) The file below was copied directly from the
-- Hackage repo.  I only made some minimal formatting / documentation changes,
-- since this utility seemed like a good case study in multithreaded Haskell.

module RateLimit (
    generateRateLimitedFunction
  , RateLimit(..)
  , ResultsCombiner
  , dontCombine
  , rateLimitInvocation
  , rateLimitExecution
  ) where

import Control.Concurrent
    ( threadDelay, forkIO, newEmptyMVar, putMVar, takeMVar, MVar )
import Control.Concurrent.STM
    ( atomically,
      isEmptyTChan,
      newTChan,
      readTChan,
      unGetTChan,
      writeTChan,
      TChan )
import Control.Monad (void)
import Data.Functor (($>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Units ( Picosecond, TimeUnit(toMicroseconds) )

-- | The rate at which to limit an action.
data RateLimit a
  = PerInvocation a -- ^ Rate limit the action to invocation once per time
                    -- unit. With this option, the time it takes for the
                    -- action to take place is not taken into consideration
                    -- when computing the rate, only the time between
                    -- invocations of the action. This may cause the action
                    -- to execute concurrently, as an invocation may occur
                    -- while an action is still running.
  | PerExecution a  -- ^ Rate limit the action to execution once per time
                    -- unit. With this option, the time it takes for the
                    -- action to take plase is taken into account, and all
                    -- actions will necessarily occur sequentially. However,
                    -- if your action takes longer than the time unit given,
                    -- then the rate of execution will be slower than the
                    -- given unit of time.

-- | In some cases, if two requests are waiting to be run, it may be possible
-- to combine them into a single request and thus increase the overall
-- bandwidth. The rate limit system supports this, but requires a little
-- additional information to make everything work out right. You may also
-- need to do something a bit wonky with your types to make this work ...
-- sorry.
--
-- The basic idea is this: Given two requests, you can either return Nothing
-- (signalling that the two requests can be combined), or a Just with a new
-- request representing the combination of the two requests. In addition, you
-- will need to provide a function that can turn the response to this single
-- request into two responses, one for each of the original requests.
--
-- I hope this description helps you work through the type, which I'll admit
-- is a bit opaque.
type ResultsCombiner req resp = req -> req -> Maybe (req, resp -> (resp, resp))

dontCombine :: ResultsCombiner a b
dontCombine _ _ = Nothing

-- | Rate limit the invocation of a given action. This is equivalent to calling
-- 'generateRateLimitedFunction' with a 'PerInvocation' rate limit and the
-- 'dontCombine' combining function.
rateLimitInvocation :: TimeUnit t
                    => t
                    -> (req -> IO resp)
                    -> IO (req -> IO resp)
rateLimitInvocation pertime action =
  generateRateLimitedFunction (PerInvocation pertime) action dontCombine

-- | Rate limit the execution of a given action. This is equivalent to calling
-- 'generateRateLimitedFunction' with a 'PerExecution' rate limit and the
-- 'dontCombine' combining function.
rateLimitExecution :: TimeUnit t
                   => t
                   -> (req -> IO resp)
                   -> IO (req -> IO resp)
rateLimitExecution pertime action =
  generateRateLimitedFunction (PerExecution pertime) action dontCombine

-- | The most generic way to rate limit an invocation.
generateRateLimitedFunction :: forall req resp t
                             . TimeUnit t
                            => RateLimit t
                               -- ^ What is the rate limit for this action
                            -> (req -> IO resp)
                               -- ^ What is the action you want to rate limit,
                               -- given as an a MonadIO function from requests
                               -- to responses?
                            -> ResultsCombiner req resp
                               -- ^ A function that can combine requests if
                               -- rate limiting happens. If you cannot combine
                               -- two requests into one request, we suggest
                               -- using 'dontCombine'.
                            -> IO (req -> IO resp)
generateRateLimitedFunction ratelimit action combiner = do
  print "making new Tchan"
  chan <- atomically newTChan
  void $ forkIO $ runner Nothing 0 chan
  return $ resultFunction chan

  where
  currentMicroseconds :: IO Integer
  currentMicroseconds =
    toMicroseconds . (fromIntegral :: Int -> Picosecond) . fromEnum <$>
      getPOSIXTime

  -- runner: Repeatedly run requests from the channel, keeping track of the
  -- time immediately before the last request, and a "sleep discount" allowance
  -- we can spend (i.e. reduce future sleep times) based on the amount of time
  -- we've "overslept" in the past.
  runner :: Maybe Integer -> Integer -> TChan (req, MVar resp) -> IO a
  runner mLastRun lastAllowance chan = do
    (req, respMV) <- atomically $ readTChan chan
    let baseHandler resp = putMVar respMV resp

    -- should we wait for some amount of time before running?
    beforeWait <- currentMicroseconds
    let targetPeriod     = toMicroseconds $ getRate ratelimit
        timeSinceLastRun = case mLastRun of
          Just lastRun -> beforeWait - lastRun
          Nothing      -> negate targetPeriod
        targetDelay      = targetPeriod - timeSinceLastRun - lastAllowance

    -- sleep if necessary; determine sleep-discount allowance for next round
    nextAllowance <- if targetDelay < 0
      then pure $ abs targetDelay -- we have more allowance left
      else do
        -- sleep for *at least* our target delay time
        threadDelay $ fromIntegral targetDelay
        afterWait <- currentMicroseconds
        let slept     = afterWait - beforeWait
            overslept = slept - targetDelay
        return overslept

    -- before running, can we combine this with any other requests on the pipe?
    (req', finalHandler) <- updateRequestWithFollowers chan req baseHandler
    let run = action req' >>= finalHandler

    beforeRun <- currentMicroseconds
    if shouldFork ratelimit
      then void $ forkIO run
      else run

    runner (Just beforeRun) nextAllowance chan

  -- updateRequestWithFollowers: We have one request. Can we combine it with
  -- some other requests into a cohesive whole?
  updateRequestWithFollowers :: TChan (req, MVar resp)
                             -> req
                             -> (resp -> IO ())
                             -> IO (req, (resp -> IO ()))
  updateRequestWithFollowers chan req handler = do
    isEmpty <- atomically $ isEmptyTChan chan
    if isEmpty
      then return (req, handler)
      else do mCombinedAndMV <- atomically $ do
                tup@(next, nextRespMV) <- readTChan chan
                case combiner req next of
                  Nothing -> unGetTChan chan tup $> Nothing
                  Just combined -> return $ Just (combined, nextRespMV)

              case mCombinedAndMV of
                Nothing ->
                  return (req, handler)
                Just ((req', splitResponse), nextRespMV) ->
                  updateRequestWithFollowers chan req' $ \resp -> do
                    let (theirs, mine) = splitResponse resp
                    putMVar nextRespMV mine
                    handler theirs

  -- shouldFork: should we fork or execute the action in place?
  shouldFork :: RateLimit t -> Bool
  shouldFork (PerInvocation _) = True
  shouldFork (PerExecution _)  = False

  -- getRate: what is the rate of this action?
  getRate :: RateLimit t -> t
  getRate (PerInvocation x) = x
  getRate (PerExecution  x) = x

  -- resultFunction: the function (partially applied on the channel) that will
  -- be returned from this monstrosity.
  resultFunction :: TChan (req, MVar resp) -> req -> IO resp
  resultFunction chan req = do
    respMV <- newEmptyMVar
    -- write the empty MVar to the channel, indicating
    -- that we are waiting for the action to finish
    atomically $ writeTChan chan (req, respMV)
    -- takeMVar waits until the MVar is non-empty before reading from it
    takeMVar respMV