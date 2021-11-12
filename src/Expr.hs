{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr where

import CC.Syntax
import CC.Semantics hiding (Dec)
import Data.List (nub)
import Data.Function (fix)
import Data.Generics
import Language.Haskell.TH
import Metalift

-- data Expr = Num Int | Add Expr Expr
-- exprD :: Q Dec
exprD = [d|
  data Expr = Num Int
            | Add Expr Expr
  |]


liftExpr decl = do
  [DataD [] d [] Nothing cs []] <- decl
  name <- newName "VExpr"
  let c = NormalC name
                  [ ( Bang NoSourceUnpackedness
                           NoSourceStrictness
                    , AppT (ConT ''V) (ConT d)
                    )
                  ]
  pure [DataD [] d [] Nothing (cs ++ [c]) []]

liftedExprD = liftExpr exprD

-- getLiterals :: Expr -> V [Int]
-- getLiterals (Num x) = pure [x]
-- getLiterals (Add e1 e2) = (++) <$> getLiterals e1 <*> getLiterals e2
-- getLiterals (VExpr ve) = ve >>= getLiterals
--
-- vGetLiterals :: V Expr -> V [Int]
-- vGetLiterals = (>>= getLiterals)
--
-- -- countUniqueLiterals :: V Expr -> V Int
-- -- countUniqueLiterals ve = length . nub <$> vGetLiterals ve
--
-- -- countUniqueLiterals :: Expr -> V Int
-- countUniqueLiterals e =
--   length . nub
--   <$> fix (\f e -> case e of
--                      Num x -> pure [x]
--                      Add e1 e2 -> (++) <$> f e1 <*> f e2
--                      VExpr ve -> ve >>= f) e


-- ast :: Expr
-- ast =
--   Add
--     (Add (Num 1) (Add (Num 2) (Num 3)))
--     (Add (Add (Num 3) (Num 4)) (Num 5))
--
-- vast :: Expr
-- vast =
--   Add
--     (Add (Num 1) (Add (Num 2) (Num 3)))
--     (Add (Add (Num 3) (Num 4)) (Num 5))
