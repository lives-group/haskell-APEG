{-# LANGUAGE GADTs,
             TypeFamilies,
             KindSignatures,
             TypeOperators,
             DataKinds,
             PolyKinds,
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

data PExp (env :: [(Symbol,*)]) :: * -> * where
  Term :: String -> PExp env String
  NonTerm :: (Lookup s env ~ 'Just t) => Sing s -> PExp env t
  Cat :: PExp env (a -> b) -> PExp env a -> PExp env b
  Choice :: PExp env a -> PExp env a -> PExp env a
  Neg :: PExp env a -> PExp env ()
  Star :: PExp env a -> PExp env [a]
  Success :: a -> PExp env a
  Failure ::  String -> PExp env a
  Map :: (a -> b) -> PExp env a -> PExp env b
  Bind :: PExp env a -> (a -> PExp env' b) -> PExp env' b


data PEG (env :: [(Symbol,*)]) where
  Nil  :: PEG '[]
  Cons :: (Lookup s' env ~ 'Nothing) =>
          (Sing s' ,
           PExp ('(s', a) ': env) a) ->
          PEG env ->
          PEG ('( s',a) ': env)

data Ex2 (p :: k -> k' -> *) where
  Ex2 :: p e i -> Ex2 p

lookupEnv :: Lookup s env ~ 'Just t => Sing s -> PEG env -> Ex2 PExp
lookupEnv s (Cons (s',e) env)
  = case s %:== s' of
       STrue -> Ex2 e
       SFalse -> lookupEnv s env

instance  Functor (PExp env) where
  fmap = Map

instance  Applicative (PExp env) where
  pure = Success
  (<*>) = Cat

instance Alternative (PExp env) where
  empty = Failure "empty"
  (<|>) = Choice

instance  Monad (PExp env) where
  return = Success
  (>>=) = Bind
  fail = Failure
