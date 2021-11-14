{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr where

import CC.Syntax
import CC.Semantics hiding (Dec)
import Data.List (nub)
import Data.Function (fix)
import Data.Generics
import Language.Haskell.TH
import Metalift

exprD :: DecsQ
exprD = [d|
  data Expr = Num Int
            | Add Expr Expr
          deriving (Show)
  |]

liftedExprD :: DecsQ
liftedExprD = fmap (fmap liftExpr) exprD
