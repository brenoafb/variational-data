{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Utils
import Language.Haskell.TH
import Expr
import Var
import Metalift

$(liftedExprD)

-- evalD :: DecsQ
-- evalD = [d|
--   eval :: Expr -> Int
--   eval (Num x) = x
--   eval (Add x y) = (+) (eval x) (eval y)
--   |]

-- eval :: Expr -> V Expr
-- eval (App e1 e2) = liftM2 apply (eval e1) (eval e2)
-- eval (VExpr ve) = ve >>= eval
-- eval x = pure x
--
-- apply :: Expr -> Expr -> V Expr
-- apply (Abs v b) x = eval (subs v x b)
-- apply e@(App t1 t2) x = liftM2 apply (eval e) (pure x)
--
subs :: String -> Expr -> Expr -> Expr
subs v t (Atom y)
  | v == y    = t
  | otherwise = Atom y
subs v t (Abs p b) = Abs p (subs v t b)
subs v t (App t1 t2) = App (subs v t t1) (subs v t t2)

evalApplyD = [d|
    eval :: Expr -> Expr
    eval (App e1 e2) = apply (eval e1) (eval e2)
    eval x = x

    apply :: Expr -> Expr -> Expr
    apply (Abs v b) x = eval (subs v x b)
    apply e x = apply (eval e) x
  |]

liftDecsQ :: DecsQ -> DecsQ
liftDecsQ decsq = do
  decs <- runQ decsq
  let funcData = getArities decs
  pure $ concatMap (liftFunD funcData) decs


liftedEvalApplyD = liftDecsQ evalApplyD
