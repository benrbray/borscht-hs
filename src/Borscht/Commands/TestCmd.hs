{-# LANGUAGE FlexibleInstances #-}


module Borscht.Commands.TestCmd where

import Borscht.Util.Fuzzy (distanceCosine)
import Borscht.Util.Normalize (normalized)
--import Borscht.Util.Kakasi (fastSin, kakasiConfigure, kakasiConvert, kakasiClose)

import Data.Text (Text)
import qualified Data.Text as Text


import Data.Monoid (First(..), (<>))
import Control.Applicative (liftA2)

import Data.Foldable (asum)
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)

------------------------------------------------------------

import Byline.Menu
import qualified Data.List.NonEmpty as NonEmpty

-- | Menu items that we'll ask the user to choose from.
data Item
  = Fruit Text
  | Vegetable Text
  deriving (Show)

-- | How to display a menu item.
instance ToStylizedText Item where
  toStylizedText item = case item of
    Fruit name -> text name <> (" (fruit)" <> fg red)
    Vegetable name -> text name <> (" (vegetable)" <> fg green)

-- | The list of menu items.
items :: NonEmpty.NonEmpty Item
items =
  NonEmpty.fromList
    [ Fruit "Watermelon",
      Vegetable "Cucumber",
      Fruit "Kiwi",
      Vegetable "Asparagus"
    ]

-- | It's main!
runTestCmd :: IO ()
runTestCmd = do
  let menuConfig =
        menuBanner ("Pick a snack: " <> bold) $
          menu items
      prompt = "Which snack? " <> bold <> fg yellow
      onError = "Please pick a valid item!" <> fg red

  -- Display the menu and get back the item the user selected.  The
  -- user will be able to select an item using it's index, name, or
  -- using tab completion.
  answer <-
    runBylineT $
      askWithMenuRepeatedly menuConfig prompt onError

  putStrLn ("You picked: " ++ show answer)

------------------------------------------------------------

-- runTestCmd :: IO ()
-- runTestCmd = do
--     -- convert string
--     print $ normalized "東京に住んでいます"
--     print $ distanceCosine 3 "mossissippi" "Mississippi"

-- testKakasi :: IO ()
-- testKakasi = do
--     -- initialize kakasi
--     status <- kakasiConfigure ["-i", "utf8", "-o", "utf8", "-Ja", "-Ha", "-Ka"]
--     print status
--     -- convert string
--     -- (NOTE: currently does NOT function correctly)
--     latin <- kakasiConvert "東京に住んでいます"
--     -- close kakasi
--     kakasiClose
--     putStrLn (Text.unpack latin)

------------------------------------------------------------

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty as = Just as

lazy1 :: IO [Integer]
lazy1 = do
    putStrLn "lazy1..."
    return []

lazy2 :: IO [Integer]
lazy2 = do
    putStrLn "lazy2..."
    return [1,2,3]

lazy3 :: IO [Integer]
lazy3 = do
    putStrLn "lazy3..."
    let zs = 1:zs
    return zs

testLazy :: IO ()
testLazy = do
    let f1 = (First . nonEmpty) <$> lazy1
    let f2 = (First . nonEmpty) <$> lazy2
    let f3 = (First . nonEmpty) <$> lazy3

    let (<!>) = liftA2 (<>)
    
    r1 <- f1 <!> f2 <!> f3
    print r1

    pure ()

testFirstT :: IO ()
testFirstT = do
    let f1 = firstT $ nonEmpty <$> do { putStrLn "f1"; lazy1 }
    let f2 = firstT $ nonEmpty <$> do { putStrLn "f2"; lazy2 }
    let f3 = firstT $ nonEmpty <$> do { putStrLn "f3"; lazy3 }
    let ff = getFirstT (f1 <> f2 <> f3)

    r <- ff
    print r
    pure ()

-- Lazily-evaluated monoid instance, for trying a sequence of fallback actions.
-- Initially tried a @(First a)@, but the semigroup operation (<>) is not lazy.
-- The below works because the Alternative instance for the MaybeT monad transformer
-- returns the first successful result, without executing the remaining actions.
-- (thanks to https://stackoverflow.com/a/47126169/1444650)
newtype FirstT m a = FirstT (MaybeT m a)

firstT :: m (Maybe a) -> FirstT m a
firstT tma = FirstT (MaybeT tma)

getFirstT :: FirstT m a -> m (Maybe a)
getFirstT (FirstT (MaybeT tma)) = tma

instance Monad m => Semigroup (FirstT m a) where
    FirstT m1 <> FirstT m2 = FirstT $ m1 <|> m2

instance Monad m => Monoid (FirstT m a) where
    mempty = FirstT empty