{-# LANGUAGE RankNTypes #-}

module SYB where

import Data.Generics


data TransformType = Stop
                   | Continue
                   | SelfOnly
                   deriving (Eq, Show)

everywhereBut' :: GenericQ TransformType -> GenericT -> GenericT
everywhereBut' q f = go
  where
    go :: GenericT
    go x =
      case q x of
        SelfOnly -> f x
        Stop -> x
        Continue -> f (gmapT go x)
