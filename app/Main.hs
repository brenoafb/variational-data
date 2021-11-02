{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import CC.Syntax
import CC.Semantics
import Data.Generics
import Expr
import Text.Pretty.Simple (pPrint)
import Language.Haskell.TH

$(liftedExpr)

main :: IO ()
main = putStrLn "Hello world"
