{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import CC.Syntax
import CC.Semantics
import Data.Generics
import Text.Pretty.Simple (pPrint)
import Language.Haskell.TH
import Expr
import Eval
import Metalift
import Data.List (nub)

$(concat <$> (map liftFunD <$> evalD))

$(concat <$> (map liftFunD <$> countNumsD))

$(concat <$> (map liftFunD <$> countAddsD))

$(concat <$> (map liftFunD <$> getLiteralsD))

countUniqueLiterals :: V Expr -> V Int
countUniqueLiterals ve = length . nub <$> vgetLiterals ve

main :: IO ()
main = putStrLn "Hello world"

ast :: Expr
ast =
  Add
    (Add (Num 1) (Add (Num 2) (Num 3)))
    (Add (Add (Num 3) (Num 4)) (Num 5))

vast :: V Expr
vast =
  Dim "evens" ["yes", "no"]
  $ Obj
  $ Add
    (Add (Num 1) (Add (Num 2) (Num 3)))
    (VExpr
      $ Chc "evens"
            [ Obj (Add (Add (Num 2) (Num 4)) (Num 6))
            , Obj (Add (Add (Num 3) (Num 5)) (Num 7))
            ]
    )
