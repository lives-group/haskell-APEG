{-# LANGUAGE GADTs, 
             DeriveFunctor#-}

module Text.APEG.Syntax.APEGSyn where

import Control.Applicative

import Data.Singletons.Prelude
       
import Data.Char    


-- deep  embedding of parser expressions

-- 1. typed non-terminals

data In s env where
  Here  :: In s (env , s)
  There :: In s env -> In s (env, s')

-- 2. weakening of symbols

wk :: Sym env s -> Sym (env , a) s
wk (NonTerm p) = NonTerm (There p)
wk (Term s)    = Term s

-- 3. symbols

data Sym env s where
  Term    :: String -> Sym env String
  NonTerm :: In s env -> Sym env s

-- 4. parser expressions

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

-- weakening of parser expressions

weak :: PExp env a -> PExp (env, b) a
weak (Symb s) = Symb (wk s)
weak (Cat e e') = Cat (weak e) (weak e')
weak (Choice e e') = Choice (weak e) (weak e')
weak (Neg e) = Neg (weak e)
weak (Star e) = Star (weak e)
weak (Map f e) = Map f (weak e)
weak (Bind e f) = Bind (weak e) (\ a -> weak (f a))
weak (Success a) = Success a
weak (Error s) = Error s

-- definition of a parser expression grammar

data PEG env a = PEG {
                   start :: In a env
                 , exprs :: Env PExp env env
                 }           

-- environment and its lookup function

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
