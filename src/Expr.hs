{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr where

import Language.Haskell.TH
import Metalift

exprD :: DecsQ
exprD = [d|
    data Expr = Atom String           -- x
              | Abs  String Expr      -- λ x . <body>
              | App  Expr Expr        -- (x y)
              deriving Show
  |]

liftedExprD = exprD >>= \decs -> pure $ fmap liftExpr decs

-- TODO try to lift this instance
-- instance Show Term where
--   show (Atom x) = x
--   show (Abs v b) = "(λ " <> v <> " . " <> show b <> ")"
--   show (App t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"
