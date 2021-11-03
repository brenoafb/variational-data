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

-- eval :: Expr -> Int
-- eval (Num x) = x
-- eval (Add x y) = eval x + eval y

-- eval :: Expr -> V Int
-- eval (Num x) = pure x
-- eval (Add v1 v2) = (+) <$> eval v1 <*> eval v2
-- eval (VExpr ve) = ve >>= eval
--
-- veval :: V Expr -> V Int
-- veval ve = ve >>= eval
--
-- countNums :: Expr -> V Int
-- countNums (Num x) = pure 1
-- countNums (Add e1 e2) = (+) <$> countNums e1 <*> countNums e2
-- countNums (VExpr ve) = ve >>= countNums
--
-- countAdds :: Expr -> V Int
-- countAdds (Num x) = pure 0
-- countAdds (Add e1 e2) = (\x y -> 1 + x + y) <$> countAdds e1 <*> countAdds e2
-- countAdds (VExpr ve) = ve >>= countAdds
--
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
