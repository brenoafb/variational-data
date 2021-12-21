{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

-- import CC.Syntax
-- import CC.Semantics
import Var
import Data.Generics
import Text.Pretty.Simple (pPrint)
import Language.Haskell.TH
import Expr
import Eval
import Metalift
import Data.List (nub)

-- $(concat <$> (map liftFunD <$> evalD))
--
-- $(concat <$> (map liftFunD <$> countNumsD))
--
-- $(concat <$> (map liftFunD <$> countAddsD))
--
-- $(concat <$> (map liftFunD <$> getLiteralsD))

$(liftedEvalApplyD)

-- countUniqueLiterals :: V Expr -> V Int
-- countUniqueLiterals ve = length . nub <$> vgetLiterals ve

-- ast :: Expr
-- ast =
--   Add
--     (Add (Num 1) (Add (Num 2) (Num 3)))
--     (Add (Add (Num 3) (Num 4)) (Num 5))

vast :: V Expr
vast =
  Chc "A" (Obj $ Atom "a") (Obj $ App (Abs "x" (Atom "x")) (Atom "b"))
