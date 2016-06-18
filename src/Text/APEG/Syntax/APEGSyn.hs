{-# LANGUAGE GADTs,
             TypeFamilies,
             KindSignatures,
             TypeOperators,
             DataKinds,
             RankNTypes,
             FlexibleInstances,
             DeriveFunctor #-}

module Text.APEG.Syntax.APEGSyn where


import Control.Applicative
import Control.Monad (replicateM, guard)

import Data.Singletons.Prelude
import Data.Singletons.Prelude.List

import Data.Char

import GHC.TypeLits

-- deep  embedding of parser expressions

data PExp (env :: [Symbol]) (tys :: [*]) :: * -> * where
  Term :: String -> PExp env tys String
  NonTerm :: ( Length env ~ Length tys
             , ElemIndex s env ~ 'Just n
             , KnownSymbol s
             , (tys :!! n) ~ t) => Sing s -> PExp env tys t
  Cat :: PExp env tys (a -> b) -> PExp env tys a -> PExp env tys b
  Choice :: PExp env tys a -> PExp env tys a -> PExp env tys a
  Neg :: PExp env tys a -> PExp env tys ()
  Star :: PExp env tys a -> PExp env tys [a]
  Success :: a -> PExp env tys a
  Failure ::  String -> PExp env tys a
  Map :: (a -> b) -> PExp env tys a -> PExp env tys b
  Bind :: PExp env tys a -> (a -> PExp env' tys' b) -> PExp env' tys' b


instance  Functor (PExp env tys) where
  fmap = Map

instance  Applicative (PExp env tys) where
  pure = Success
  (<*>) = Cat

instance  Monad (PExp env tys) where
  return = Success
  (>>=) = Bind
  fail = Failure
