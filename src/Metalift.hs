{-# LANGUAGE TemplateHaskell #-}

module Metalift where

import Language.Haskell.TH
import Data.Generics

liftFunD :: Dec -> Dec
liftFunD (FunD name clauses) = FunD name $ liftedClauses ++ [varClause]
  where liftedClauses = map metaliftC clauses
        ve = mkName "ve"
        varClause = Clause [ConP (mkName "VExpr") [VarP ve]]
                           (NormalB (InfixE (Just (VarE ve))
                                            (VarE (mkName ">>="))
                                            (Just (VarE name)))) []

-- eval (VExpr ve) = ve >>= eval

metaliftD :: Dec -> Dec
metaliftD = everywhere $ mkT metaliftB

metaliftC :: Clause -> Clause
metaliftC = everywhere $ mkT metaliftB

liftT :: Type -> Type
liftT = undefined

metaliftB :: Body -> Body
-- lift plain variables into applicative
metaliftB (NormalB (VarE x)) = NormalB (AppE (VarE $ mkName "pure") (VarE x))
-- lift binary function application
metaliftB (NormalB (InfixE x op y)) =
  NormalB
  (InfixE (Just (InfixE (Just op)
                        (VarE $ mkName "<$>")
                        x))
          (VarE $ mkName"<*>")
          y)
metaliftB x = x
