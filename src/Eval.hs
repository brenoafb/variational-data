{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Language.Haskell.TH
import Expr
import Var
import Control.Monad (join)
import Metalift

(<**>) :: Monad m => m (a -> m b) -> m a -> m b
mf <**> ma = join (mf <*> ma)

liftM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftM2 f x y = join $ f <$> x <*> y

-- liftM2 f x y = f <**> x <**> y
-- liftM2 apply (eval e1) (eval e2) = apply <**> eval e1 <**> eval e2

$(liftedExprD)

-- evalD :: DecsQ
-- evalD = [d|
--   eval :: Expr -> Int
--   eval (Num x) = x
--   eval (Add x y) = (+) (eval x) (eval y)
--   |]

eval :: Expr -> V Expr
eval (App e1 e2) = _ <**> eval e1 <**> eval e2
eval (VExpr ve) = ve >>= eval
eval x = pure x

apply :: Expr -> Expr -> V Expr
apply (Atom y) x  = error "Cannot apply atom"
apply (Abs v b) x = eval $ subs v x b
apply e@(App t1 t2) x = liftM2 apply (eval e) (pure x)

subs :: String -> Expr -> Expr -> Expr
subs v t (Atom y)
  | v == y    = t
  | otherwise = Atom y
subs v t (Abs p b) = Abs p $ subs v t b
subs v t (App t1 t2) = App (subs v t t1) (subs v t t2)


evalApplyD = [d|
    eval :: Expr -> Expr
    eval (App e1 e2) = apply (eval e1) (eval e2)
    eval (Atom s) = Atom s
    eval (Abs s e) = Abs s e

    apply :: Expr -> Expr -> Expr
    apply (Abs v b) x = eval $ subs v x b
    apply e@(App _ _) x = apply (eval e) x
  |]

liftDecsQ :: DecsQ -> DecsQ
liftDecsQ decsq = do
  decs <- runQ decsq
  let names = getNames decs
  pure $ concatMap (liftFunD names) decs


liftedEvalApplyD = liftDecsQ evalApplyD
