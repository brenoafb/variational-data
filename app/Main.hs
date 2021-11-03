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

$(map liftFunD <$> evalD)

main :: IO ()
main = putStrLn "Hello world"
