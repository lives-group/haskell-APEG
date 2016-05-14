{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}

module Text.APEG.Syntax.APEGSyn where

import Control.Applicative
import Control.Monad
    
import Data.Singletons.Prelude   
import Data.Singletons.Prelude.List

import GHC.TypeLits
    
type family At (k :: Symbol) (xs :: [(Symbol, a)]) where
    At x ('(x,y) ': xs) = y
    At x ('(z,y) ': xs) = At x xs
  
data APEG (env :: [(Symbol,*)]) (a :: *) where
  Chr :: Char -> APEG env Char
  Any :: APEG env Char
  Symb :: String -> APEG env String       
  Success :: a -> APEG env a       
  Failure :: APEG env a
  Not :: APEG env a -> APEG env ()
  Cat :: APEG env (a -> b) -> APEG env a -> APEG env b
  Choice :: APEG env a -> APEG env a -> APEG env a
  Star :: APEG env a -> APEG env [a]
  Map :: (a -> b) -> APEG env a -> APEG env b
  Atrib :: KnownSymbol s => proxy s -> (a -> APEG env b) -> APEG ('(s,b) ': env) ()
  Get :: KnownSymbol s => proxy s -> APEG env (s `At` env)
  Bind :: APEG env a -> (a -> APEG env b) -> APEG env b       

instance Functor (APEG env) where
    fmap = Map

instance Applicative (APEG env) where
    pure = Success
    (<*>) = Cat

instance Alternative (APEG env) where
    (<|>) = Choice
    empty = Failure

instance Monad (APEG env) where
    return = Success
    (>>=) = Bind

instance MonadPlus (APEG env) where
    mplus = (<|>)
    mzero = empty        
            
chr :: Char -> APEG env Char
chr = Chr

any :: APEG env Char
any = Any

neg :: APEG env a -> APEG env ()
neg = Not

star :: APEG env a -> APEG env [a]
star p = Star p

string :: String -> APEG env String
string = Symb          

atrib :: KnownSymbol s => proxy s -> (a -> APEG env b) -> APEG ('(s,b) ': env) ()
atrib = Atrib         

get :: KnownSymbol s => proxy s -> APEG env (s `At` env)
get = Get
