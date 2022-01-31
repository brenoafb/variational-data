module Utils where

import Control.Monad (join)

(<**>) :: Monad m => m (a -> m b) -> m a -> m b
mf <**> ma = join (mf <*> ma)

liftM1 :: Monad m => (a -> m b) -> m a -> m b
liftM1 = (=<<)

liftM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftM2 f x y = join $ f <$> x <*> y
