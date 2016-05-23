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

type family Update (x :: k) (s :: v) (xs :: [(k, v)]) :: [(k,v)] where
  Update x s '[] = '[]
  Update x s ('(x' , v) ': xs) = If ((x :== x') :&& (s :== v)) ('(x,s) ': xs) ('(x', v) ': Update x s xs)    

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
foo = Set (sing :: Sing "a") True 

foo' :: PExp '[ '("a", Bool), '("b", Char)] ()
foo' = Set (sing :: Sing "b") 'a' 

      
mytest1 :: APeg '[ '("a", Bool), '("b", Char)] Char      
mytest1 = APeg (((\_ _ c -> c) <$> foo <*> foo' <*> Get (sing :: Sing "b")))

-- more tests

digit :: PExp env Char
digit = Sat isDigit         

number :: PExp env Int
number = f <$> Star digit
         where
           f = foldl (\a b -> a * 10 + b) 0 . map g
           g c = ord c - ord '0'                 
