{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Language.Haskell.TH
import Expr

$(liftedExprD)

evalD :: DecsQ
evalD = [d|
  eval :: Expr -> Int
  eval (Num x) = x
  eval (Add x y) = (+) (eval x) (eval y)
  eval (If c t e) =
    if (==) (eval c) 0
    then eval e
    else eval t
  |]

countNumsD :: DecsQ
countNumsD = [d|
  countNums :: Expr -> Int
  countNums (Num x) = 1
  countNums (Add e1 e2) = (+) (countNums e1) (countNums e2)
  countNums (If c t e) =
    (\x y z -> x + y + z) (countNums c) (countNums t) (countNums e)
  |]

countAddsD :: DecsQ
countAddsD = [d|
  countAdds :: Expr -> Int
  countAdds (Num _) = 0
  countAdds (Add e1 e2) =
    (\x y -> x + y + 1) (countAdds e1) (countAdds e2)
  countAdds (If c t e) =
    (\x y z -> x + y + z) (countAdds c) (countAdds t) (countAdds e)
  |]

getLiteralsD :: DecsQ
getLiteralsD = [d|
  getLiterals :: Expr -> [Int]
  getLiterals (Num x) = [x]
  getLiterals (Add e1 e2) = (++) (getLiterals e1) (getLiterals e2)
  getLiterals (If c t e) =
    (\x y z -> x ++ y ++ z) (getLiterals c) (getLiterals t) (getLiterals e)
  |]
