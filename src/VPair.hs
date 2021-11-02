module VPair where

import CC.Syntax

type VPair a b = V (a, b)

type VPair' a b = (V a, V b)

favoriteNumbers :: VPair Int Int
favoriteNumbers =
  Dim "Even" ["yes", "no"]
  $ Chc "Even" [Obj (2, 4), Obj (1, 3)]

favoriteNumbers' :: VPair' Int Int
favoriteNumbers' =
  ( Dim "Even" ["yes", "no"] $ Chc "Even" [Obj 2, Obj 1]
  , Dim "Even" ["yes", "no"] $ Chc "Even" [Obj 4, Obj 3]
  )

sumNumbers :: VPair Int Int -> V Int
sumNumbers vp = vp >>= (\(x, y) -> pure $ x + y)

sumNumbers' :: VPair' Int Int -> V Int
sumNumbers' (v1, v2) = (+) <$> v1 <*> v2

