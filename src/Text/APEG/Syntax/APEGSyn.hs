{-# LANGUAGE GADTs, 
             DataKinds, 
             PolyKinds, 
             TypeOperators, 
             TypeFamilies, 
             ScopedTypeVariables, 
             ConstraintKinds, 
             UndecidableInstances #-}

module Text.APEG.Syntax.APEGSyn where

import Control.Applicative
    
import Data.Char    
import Data.Proxy    
import Data.Singletons.Prelude   
import Data.Singletons.Prelude.List

data PExp (env :: [(Symbol, *)]) (a :: *) where
     Sat  :: (Char -> Bool) -> PExp env Char
     Symb :: String -> PExp env String
     Success :: a -> PExp env a
     Map :: (a -> b) -> PExp env a -> PExp env b
     Bind :: PExp env a -> (a -> PExp env b) -> PExp env b
     Failure :: String -> PExp env a
     Not :: PExp env a -> PExp env ()
     Cat :: PExp env (a -> b) -> PExp env a -> PExp env b
     Choice :: PExp env a -> PExp env a -> PExp env a
     Star :: PExp env a -> PExp env [a]
     Set :: (Lookup s env ~ 'Just t) => Sing s -> t -> PExp env ()
     Get :: (Lookup s env ~ 'Just t) => Sing s -> PExp env t
     Check :: (Lookup s env ~ 'Just t) => Sing s -> (t -> Bool) -> PExp env ()

newtype APEG (env :: [(Symbol, *)]) (a :: *) = APEG { runApeg :: PExp ('("lang", PExp env a) ': env) a }

instance Functor (PExp env) where
    fmap = Map

instance Applicative (PExp env) where
    pure = Success
    (<*>) = Cat

instance Alternative (PExp env) where
    empty = Failure "empty"
    (<|>) = Choice

instance Monad (PExp env) where
    return = pure
    (>>=) = Bind
    fail = Failure
    

