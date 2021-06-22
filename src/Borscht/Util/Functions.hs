module Borscht.Util.Functions where

------------------------------------------------------------

-- text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Lazy.Builder as TLB (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as TLB (decimal)

------------------------------------------------------------

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g a = (f a, g a)

dupe :: a -> (a,a)
dupe x = (x,x)

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (x,y) = (f x, y)

mapSnd :: (b -> d) -> (a,b) -> (a,d)
mapSnd g (x,y) = (x, g y)

------------------------------------------------------------

-- https://github.com/haskell/text/issues/218
intToText :: Integral a => a -> T.Text
intToText = TL.toStrict . TLB.toLazyText . TLB.decimal