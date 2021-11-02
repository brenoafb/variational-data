module VIdentity where

import CC.Syntax

newtype Identity a = Identity a deriving (Eq, Show)

type VIdentity a = V (Identity a)

type VIdentity' a = Identity (V a)

myNumber :: VIdentity Int
myNumber =
  Dim "Even" ["yes", "no"]
  $ Chc "Even" [Obj $ Identity 2, Obj $ Identity 1]

myNumber' :: VIdentity' Int
myNumber' =
  Identity
  $ Dim "Even" ["yes", "no"]
  $ Chc "Even" [Obj 2, Obj 1]

mySucc :: VIdentity Int -> V Int
mySucc vi = vi >>= (\(Identity x) -> pure $ x + 1)

mySucc' :: VIdentity' Int -> V Int
mySucc' (Identity vi) = (+1) <$> vi
