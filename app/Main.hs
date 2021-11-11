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

$(concat <$> (map liftFunD <$> evalD))

$(concat <$> (map liftFunD <$> countNumsD))

-- $(concat <$> (map liftFunD <$> countAddsD))

main :: IO ()
main = putStrLn "Hello world"
