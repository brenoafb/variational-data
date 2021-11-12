{-# LANGUAGE TemplateHaskell #-}

module Metalift where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Generics
import SYB

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
mkvfun name = [ValD (VarP vname) body []]
  where vname = prependName "v" name
        body = NormalB (InfixE Nothing
                               (VarE (mkName ">>="))
                               (Just (VarE name)))

appendName :: Name -> String -> Name
appendName (Name occName _) s' = mkName $ s <> s'
  where s = occString occName

prependName :: String -> Name -> Name
prependName s' (Name occName _) = mkName $ s' <> s
  where s = occString occName

metaliftBut :: Data a => Name -> a -> a
metaliftBut name = everywhereBut' (Continue `mkQ` shouldExclude) (mkT metaliftE)
  where shouldExclude (VarE name') =
          if name == name' then Stop else Continue
        shouldExclude (AppE (VarE name') _) =
          if name == name' then Stop else Continue
        shouldExclude (LamE _ _) = SelfOnly
        shouldExclude (ListE _) = SelfOnly
        shouldExclude _ = Continue

metaliftE :: Exp -> Exp
metaliftE (VarE x) = AppE (VarE $ mkName "pure") (VarE x)
metaliftE (LitE l) = AppE (VarE $ mkName "pure") (LitE l)
metaliftE (AppE f x) = InfixE (Just f) (VarE $ mkName "<*>") (Just x)
metaliftE (ListE l) = AppE (VarE $ mkName "pure") (ListE l)
metaliftE x = x
