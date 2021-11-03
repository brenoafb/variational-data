{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Data.Generics
import Language.Haskell.TH
import Metalift
import Expr

$(liftedExprD)

evalD = [d|
--  eval :: Expr -> Int
  eval (Num x) = x
  eval (Add x y) = eval x + eval y
  |]
