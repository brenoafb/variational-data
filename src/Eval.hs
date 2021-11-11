{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Data.Generics
import Language.Haskell.TH
import Metalift
import Expr

$(liftedExprD)

evalD = [d|
  eval :: Expr -> Int
  eval (Num x) = x
  eval (Add x y) = (+) (eval x) (eval y)
  |]

countNumsD = [d|
  countNums :: Expr -> Int
  countNums (Num x) = 1
  countNums (Add e1 e2) = (+) (countNums e1) (countNums e2)
  |]

countAddsD = [d|
  countAdds :: Expr -> Int
  countAdds (Num _) = 0
  countAdds (Add e1 e2) = (\x y -> 1 + x + y) (countAdds e1) (countAdds e2)
  |]
