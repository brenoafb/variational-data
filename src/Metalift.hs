{-# LANGUAGE TemplateHaskell #-}

module Metalift where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Generics

liftFunD :: Dec -> [Dec]
liftFunD (SigD name (AppT l t)) = [SigD name (AppT l vt)]
  where vt = AppT (ConT $ mkName "V") t
liftFunD (FunD name clauses) = [FunD name $ liftedClauses ++ [varClause]] ++ mkvfun name
  where liftedClauses = map (metaliftBut name) clauses
        ve = mkName "ve"
        varClause = Clause [ConP (mkName "VExpr") [VarP ve]]
                           (NormalB (InfixE (Just (VarE ve))
                                            (VarE (mkName ">>="))
                                            (Just (VarE name)))) []
liftFunD x = [x]

mkvfun :: Name -> [Dec]
mkvfun name = [SigD vname typ, ValD (VarP vname) body []]
  where vname = prependName "v" name
        typ = AppT (AppT ArrowT vexprT) vintT
        vexprT = (AppT (ConT $ mkName "V") (ConT $ mkName "Expr"))
        vintT  = (AppT (ConT $ mkName "V") (ConT $ mkName "Int"))
        body = NormalB (InfixE Nothing (VarE (mkName ">>=")) (Just (VarE name)))

appendName :: Name -> String -> Name
appendName (Name occName _) s' = mkName $ s <> s'
  where s = occString occName

prependName :: String -> Name -> Name
prependName s' (Name occName _) = mkName $ s' <> s
  where s = occString occName

metaliftBut :: Data a => Name -> a -> a
metaliftBut name = everywhereBut (False `mkQ` shouldExclude) (mkT metaliftE)
  where shouldExclude (VarE name') = name == name'
        shouldExclude (AppE (VarE name') _) = name == name'
        shouldExclude (LamE _ _) = True
        shouldExclude _ = False

metaliftE :: Exp -> Exp
metaliftE (VarE x) = AppE (VarE $ mkName "pure") (VarE x)
metaliftE (LitE l) = AppE (VarE $ mkName "pure") (LitE l)
metaliftE (AppE f x) = InfixE (Just f) (VarE $ mkName "<*>") (Just x)
metaliftE x = x
