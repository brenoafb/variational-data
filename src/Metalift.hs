{-# LANGUAGE TemplateHaskell #-}

module Metalift where

-- import CC.Syntax hiding (Name)
import Utils
import Var
import Language.Haskell.TH
    ( mkName,
      Exp(ConE, LitE, InfixE, ListE, LamE, AppE, VarE),
      Clause(Clause),
      Pat(VarP, ConP),
      Type(ConT, ArrowT, AppT),
      Dec(DataD, SigD, FunD),
      Name,
      Bang(Bang),
      Body(NormalB),
      Con(NormalC),
      SourceStrictness(NoSourceStrictness),
      SourceUnpackedness(NoSourceUnpackedness) )
import Language.Haskell.TH.Syntax
import Data.Generics
import SYB
import Data.List (nub)

getNames :: [Dec] -> [Name]
getNames =
  nub . concatMap (everything (++) ([] `mkQ` getName))

getName :: Dec -> [Name]
getName (SigD name _) = [name]
getName (FunD name _) = [name]
getName _ = []

getArities :: [Dec] -> [(Name, Int)]
getArities =
  nub . concatMap (everything (++) ([] `mkQ` getArity))

getArity :: Dec -> [(Name, Int)]
getArity (SigD name typ) = [(name, typeArity typ)]
getArity _ = []

typeArity :: Type -> Int
typeArity = everything (+) (0 `mkQ` getArrowT)
  where getArrowT ArrowT = 1
        getArrowT _ = 0

liftFunD :: [(Name, Int)] -> Dec -> [Dec]
-- liftFunD funcData (SigD name (AppT l t)) = [SigD name (AppT l vt)]
--   where vt = AppT (ConT $ mkName "V") t
-- liftFunD funcData (SigD name (AppT l t)) = []
-- liftFunD funcData (FunD name clauses) = [FunD name liftedClauses]
--    where liftedClauses = map (metalift funcData) clauses
--          ve = mkName "ve"
liftFunD funcData (FunD name clauses) =
  case lookup name funcData of
    Nothing -> []
    Just n  -> [FunD name $ liftedClauses ++ [varClause]]
      where liftedClauses = map (metalift funcData) clauses
            vNames = map (\k -> mkName $ "ve" ++ show k) [1..n]
            vParams = map (\vname -> ConP (mkName "VExpr") [VarP vname]) vNames
            liftM' = VarE $ mkName $ "liftM" ++ show n
            apps = foldr (\x acc -> AppE acc (VarE x)) (AppE liftM' (VarE name)) vNames
            varClause = Clause vParams
                        (NormalB apps) []
liftFunD funcData (SigD name (AppT l t)) = []
liftFunD _ x = [x]

metalift :: Data a => [(Name, Int)] -> a -> a
metalift funcData = -- everywhere (mkT (metaliftE funcData))
  everywhereBut' (Continue `mkQ` shouldExclude) (mkT (metaliftE funcData))
  where shouldExclude (VarE name) =
          if name `elem` names then Stop else Continue
        shouldExclude (AppE (VarE name) _) =
          if name `elem` names then Stop else Continue
        shouldExclude (LamE _ _) = SelfOnly
        shouldExclude (ListE _) = SelfOnly
        shouldExclude _ = Continue
        names = map fst funcData

metaliftE :: [(Name, Int)] -> Exp -> Exp
metaliftE funcData (VarE x) = AppE (VarE $ mkName "pure") (VarE x)
metaliftE funcData (LitE l) = AppE (VarE $ mkName "pure") (LitE l)
metaliftE funcData expr@(AppE (AppE f2@(VarE f2name) x) y) =
  case lookup f2name funcData of
    Just 2  -> AppE (AppE (AppE liftM2' f2) x) y
    _       -> expr
    -- Nothing -> AppE (AppE (AppE liftA2' f2) x) y
    -- _       -> error $ "function " <> show f2name <> " does not seem to have the right arity"
  where liftM2' = VarE $ mkName "liftM2"
        liftA2' = VarE $ mkName "liftA2"
metaliftE funcData (AppE f@(VarE fName) x) =
  case lookup fName funcData of
    Nothing -> InfixE (Just f) (VarE $ mkName "<*>") (Just x) -- f not being lifted
    Just 0  -> error "Cannot lift function application where function has 0 arity"
    Just n  -> AppE (AppE liftM' f) x
      where liftM' = VarE $ mkName $ "liftM" ++ show n
metaliftE funcData (AppE f x) = InfixE (Just f) (VarE $ mkName "<*>") (Just x)
metaliftE funcData (ListE l) = AppE (VarE $ mkName "pure") (ListE l)
metaliftE funcData (LamE a b) = AppE (VarE $ mkName "pure") (LamE a b)
metaliftE funcData x = x

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
