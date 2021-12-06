{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr where

import Language.Haskell.TH
import Metalift

exprD :: DecsQ
exprD = [d|
  data Expr = Num Int
            | Add Expr Expr
            | If Expr Expr Expr
          deriving (Show)
  |]

liftedExprD :: DecsQ
liftedExprD = fmap (fmap liftExpr) exprD
