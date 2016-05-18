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

import GHC.Exts    
import GHC.TypeLits


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
     Set :: (KnownSymbol s, Lookup s env ~ 'Just t) => proxy s -> t -> PExp env ()
     Get :: (KnownSymbol s, Lookup s env ~ 'Just t) => proxy s -> PExp env t
     Check :: (KnownSymbol s, Lookup s env ~ 'Just t) => proxy s -> (t -> Bool) -> PExp env Bool

newtype APeg (env :: [(Symbol, *)]) (a :: *) = APeg { runApeg :: PExp env a }

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
    

foo :: PExp '[ '("a", Bool), '("b", Char)] ()
foo = Set (Proxy :: Proxy "a") True       

foo' :: PExp '[ '("a", Bool), '("b", Char)] ()
foo' = Set (Proxy :: Proxy "b") 'a'       

      
mytest1 :: APeg '[ '("a", Bool), '("b", Char)] Char      
mytest1 = APeg (((\_ _ c -> c) <$> foo <*> foo' <*> Get (Proxy :: Proxy "b")))

-- more tests

digit :: PExp env Char
digit = Sat isDigit         

number :: PExp env Int
number = f <$> Star digit
         where
           f = foldl (\a b -> a * 10 + b) 0 . map g
           g c = ord c - ord '0'
