{-# LANGUAGE TemplateHaskell #-}

module Metalift where

-- import CC.Syntax hiding (Name)
import Var
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Generics
import SYB

liftFunD :: Dec -> [Dec]
liftFunD (SigD name (AppT l t)) = [SigD name (AppT l vt)]
  where vt = AppT (ConT $ mkName "V") t
liftFunD (FunD name clauses) = [FunD name $ liftedClauses ++ [varClause]]
  where liftedClauses = map (metalift name) clauses
        ve = mkName "ve"
        varClause = Clause [ConP (mkName "VExpr") [VarP ve]]
                           (NormalB (InfixE (Just (VarE ve))
                                            (VarE (mkName ">>="))
                                            (Just (VarE name)))) []
liftFunD x = [x]

liftExpr :: Dec -> Dec
liftExpr (DataD [] name [] Nothing cs ds) =
  let vname = prependName "V" name
      c = NormalC vname
                  [ ( Bang NoSourceUnpackedness
                           NoSourceStrictness
                    , AppT (ConT ''V) (ConT name)
                    )
                  ]
  in DataD [] name [] Nothing (cs ++ [c]) ds
liftExpr x = x

metalift :: Data a => Name -> a -> a
metalift name = everywhereBut' (Continue `mkQ` shouldExclude) (mkT metaliftE)
  where shouldExclude (VarE name') =
          if name == name' then Stop else Continue
        shouldExclude (AppE (VarE name') _) =
          if name == name' then Stop else Continue
        shouldExclude (LamE _ _) = SelfOnly
        shouldExclude (ListE _) = SelfOnly
        shouldExclude _ = Continue

metaliftE :: Exp -> Exp
metaliftE (AppE f x) = metaliftApp f x
metaliftE e@(ListE _) = metaliftPure e
metaliftE e@(LamE _ _) = metaliftPure e
metaliftE e@(VarE _) = metaliftPure e
metaliftE e@(LitE _) = metaliftPure e
metaliftE x = x

metaliftPure :: Exp -> Exp
metaliftPure = AppE (VarE $ mkName "pure")

metaliftApp :: Exp -> Exp -> Exp
metaliftApp f x = InfixE (Just f) (VarE $ mkName "<*>") (Just x)

appendName :: Name -> String -> Name
appendName (Name occName _) s' = mkName $ s <> s'
  where s = occString occName

prependName :: String -> Name -> Name
prependName s' (Name occName _) = mkName $ s' <> s
  where s = occString occName
