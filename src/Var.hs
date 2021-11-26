module Var where

data V a = Obj a
         | Chc Tag (V a) (V a)
         deriving Show

type Tag = String

instance Functor V where
  fmap f (Obj x) = Obj (f x)
  fmap f (Chc t x y) = Chc t (fmap f x) (fmap f y)

instance Applicative V where
  pure = Obj
  Obj f <*> c = fmap f c
  Chc t f g <*> c = reduce $ Chc t (f <*> c) (g <*> c)

instance Monad V where
  Obj x >>= f = f x
  Chc t x y >>= f = reduce $ Chc t (x >>= f) (y >>= f)

reduce :: V a -> V a
reduce (Obj x) = Obj x
reduce (Chc t x y) = Chc t (go True t x) (go False t y)
  where go _ _ (Obj x) = Obj x
        go f t (Chc t' x y)
          | t == t' = if f then go f t x else go f t y
          | otherwise = reduce (Chc t' (go f t x) (go f t y))
