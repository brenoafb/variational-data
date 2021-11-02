module VEither where

import CC.Syntax

type VEither a b = V (Either a b)

type VEither' a b = Either (V a) (V b)

nameOrAge :: Either String Int
nameOrAge = Right 23

nameOrAge' :: VEither String Int
nameOrAge' =
  Dim "Person" ["me", "someoneelse"]
  $ Chc "Person" [Obj (Left "Breno"), Obj (Right 23)]

nameOrAge'' :: VEither' String Int
nameOrAge'' =
  Right $ Dim "Person" ["me", "someoneelse"]
        $ Chc "Person" [Obj 22, Obj 23]

stringifyNameOrAge :: Either String Int -> String
stringifyNameOrAge (Left s) = "Name: " ++ s
stringifyNameOrAge (Right i) = "Age: " ++ show i

stringifyNameOrAge' :: VEither String Int -> V String
stringifyNameOrAge' v = do
  nameOrAge <- v
  pure $ stringifyNameOrAge nameOrAge

