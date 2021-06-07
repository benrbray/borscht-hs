module Borscht.Util.Functions where

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g a = (f a, g a)

dupe :: a -> (a,a)
dupe x = (x,x)

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (x,y) = (f x, y)

mapSnd :: (b -> d) -> (a,b) -> (a,d)
mapSnd g (x,y) = (x, g y)