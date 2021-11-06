{-# LANGUAGE TemplateHaskell #-}

module Metalift where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Generics

liftFunD :: Dec -> [Dec]
liftFunD (SigD name (AppT l t)) = [SigD name (AppT l vt)]
  where vt = AppT (ConT $ mkName "V") t
liftFunD (FunD name clauses) = [FunD name $ liftedClauses ++ [varClause]] ++ mkvfun name
  where liftedClauses = map metaliftC clauses
        ve = mkName "ve"
        varClause = Clause [ConP (mkName "VExpr") [VarP ve]]
                           (NormalB (InfixE (Just (VarE ve))
                                            (VarE (mkName ">>="))
                                            (Just (VarE name)))) []
mkvfun :: Name -> [Dec]
mkvfun name = [SigD vname typ, ValD (VarP vname) body []]
  where vname = prependName "v" name
        typ = AppT (AppT ArrowT vexprT) vintT
        vexprT = (AppT (ConT $ mkName "V") (ConT $ mkName "Expr"))
        vintT  = (AppT (ConT $ mkName "V") (ConT $ mkName "Int"))
        body = NormalB (InfixE Nothing (VarE (mkName ">>=")) (Just (VarE name)))

appendName :: Name -> String -> Name
appendName (Name occName f) s' = mkName $ s <> s'
  where s = occString occName

prependName :: String -> Name -> Name
prependName s' (Name occName f) = mkName $ s' <> s
  where s = occString occName

metaliftD :: Dec -> Dec
metaliftD = everywhere $ mkT metaliftB

metaliftC :: Clause -> Clause
metaliftC = everywhere $ mkT metaliftB

liftT :: Type -> Type
liftT = undefined

metaliftB :: Body -> Body
-- plain variables into applicative
metaliftB (NormalB (VarE x)) = NormalB (AppE (VarE $ mkName "pure") (VarE x))
-- plain constants into applicative
metaliftB (NormalB (LitE l)) = NormalB (AppE (VarE $ mkName "pure") (LitE l))
-- prefix binary function application
metaliftB (NormalB (AppE (AppE f x) y)) =
  NormalB
  ( InfixE
    ( Just
      ( InfixE (Just f) (VarE $ mkName "<$>") (Just x) )
    )
    ( VarE $ mkName "<*>")
    ( Just y)
  )
-- unary function application
metaliftB (NormalB (AppE f x)) =
  NormalB (AppE (AppE (VarE $ mkName "fmap") f) x)
-- infix binary function application
metaliftB (NormalB (InfixE x op y)) =
  NormalB
  (InfixE (Just (InfixE (Just op)
                        (VarE $ mkName "<$>")
                        x))
          (VarE $ mkName"<*>")
          y)
metaliftB x = x
