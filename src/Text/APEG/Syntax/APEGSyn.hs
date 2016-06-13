{-# LANGUAGE GADTs, 
             DataKinds, 
             PolyKinds, 
             TypeOperators, 
             TypeFamilies,
             ExistentialQuantification,
             ScopedTypeVariables, 
             ConstraintKinds, 
             UndecidableInstances #-}

module Text.APEG.Syntax.APEGSyn where

import Control.Applicative

import Data.Singletons.Prelude
       
import Data.Char    


-- deep  embedding of parser expressions

data In s env where
  Here  :: In s (env , s)
  There :: In s env -> In s (env, s')

data Sym env s where
  Term    :: String -> Sym env String
  NonTerm :: In s env -> Sym env s

data PExp env a where
  Symb :: Sym env a -> PExp env a
  Cat :: PExp env (a -> b) -> PExp env a -> PExp env b
  Choice :: PExp env a -> PExp env a -> PExp env a
  Neg :: PExp env a -> PExp env ()
  Star :: PExp env a -> PExp env [a]
  Map :: (a -> b) -> PExp env a -> PExp env b
  Bind :: PExp env a -> (a -> PExp env b) -> PExp env b
  Success :: a -> PExp env a
  Error :: String -> PExp env a

data PEG a = forall env. PEG (In a env)
                             (Env PExp env env)

data Env t u d where
  Empty :: Env t u ()
  Ext   :: Env t u d' -> t u a -> Env t u (d' , a)

lookupEnv :: In a env -> Env t u env -> t u a
lookupEnv Here (Ext _ t) = t
lookupEnv (There p) (Ext ts _) = lookupEnv p ts
  
-- some instances

instance Functor (PExp env) where
  fmap = Map

instance Applicative (PExp env) where
  pure = Success
  (<*>) = Cat

instance Alternative (PExp env) where
  empty = Error "empty"
  (<|>) = Choice

instance Monad (PExp env) where
  return = Success
  (>>=) = Bind
  fail = Error
