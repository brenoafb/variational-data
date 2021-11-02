{-# LANGUAGE TemplateHaskell #-}

module Metalift where

import Language.Haskell.TH
import Data.Generics

metaliftD :: [Dec] -> [Dec]
metaliftD = everywhere $ mkT metalift

metaliftE :: Exp -> Exp
metaliftE = everywhere $ mkT metalift

metalift :: Exp -> Exp
-- plain variables
metalift (VarE x) = AppE (VarE $ mkName "pure") (VarE x)
-- binary function application
metalift (InfixE x op y) =
  (InfixE (Just (InfixE (Just op)
                        (VarE $ mkName "<$>")
                        x))
          (VarE $ mkName"<*>")
          y)
metalift x = x

-- metalift :: ExpQ -> ExpQ
-- metalift e = do
--   e' <- e
--   case e' of
--     v@(VarE x) -> appE (varE $ mkName "pure") (varE x)
--     (LamE ps b) -> lamE (map pure ps) (metalift $ pure b)
