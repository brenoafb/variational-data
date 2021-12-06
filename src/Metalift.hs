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
liftFunD (FunD name clauses) = FunD name (liftedClauses ++ [varClause]) : mkvfun name
  where liftedClauses = map (metalift name) clauses
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
metaliftE (VarE x) = AppE (VarE $ mkName "pure") (VarE x)
metaliftE (LitE l) = AppE (VarE $ mkName "pure") (LitE l)
metaliftE (AppE f x) = InfixE (Just f) (VarE $ mkName "<*>") (Just x)
metaliftE (ListE l) = AppE (VarE $ mkName "pure") (ListE l)
metaliftE (LamE a b) = AppE (VarE $ mkName "pure") (LamE a b)
metaliftE (CondE c t f) =
  let x = mkName "x" -- TODO x may be captured, need to use Q monad
  in AppE ( AppE (VarE $ mkName ">>=")
                 c
          ) (LamE [VarP x] (CondE (VarE x) t f)) -- [|(\x -> if x then t else f)|]

metaliftE x = x

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


appendName :: Name -> String -> Name
appendName (Name occName _) s' = mkName $ s <> s'
  where s = occString occName

prependName :: String -> Name -> Name
prependName s' (Name occName _) = mkName $ s' <> s
  where s = occString occName
