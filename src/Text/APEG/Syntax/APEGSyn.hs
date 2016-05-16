{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeOperators, TypeFamilies, ScopedTypeVariables #-}

module Text.APEG.Syntax.APEGSyn where

import Data.Proxy    
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
  Cat :: APEG env (a -> b) -> APEG env' a -> APEG (env :++ env') b
  Choice :: APEG env a -> APEG env a -> APEG env a
  Star :: APEG env a -> APEG env [a]
  Map :: (a -> b) -> APEG env a -> APEG env b
  Atrib :: KnownSymbol s => proxy s -> b -> APEG ('(s,b) ': '[]) ()
  Get :: KnownSymbol s => proxy s -> APEG env (s `At` env)       

instance Functor (APEG env) where
    fmap = Map
            
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

atrib :: KnownSymbol s => proxy s -> b -> APEG ('(s,b) ': '[]) ()
atrib = Atrib         

get :: KnownSymbol s => proxy s -> APEG env (s `At` env)
get = Get

check :: KnownSymbol s => proxy s -> ((s `At` env) -> Bool) -> APEG env Bool
check s f = f <$> Get s         

infixl 4 <$$>
infixl 4 <**>
            
(<$$>) :: (a -> b) -> APEG env a -> APEG env b
f <$$> p = Map f p          

(<**>) :: APEG env (a -> b) -> APEG env' a -> APEG (env :++ env') b
p <**> q = Cat p q          
           
-- simple tests
            
foo :: APEG ('("a", Bool) ': '[]) ()
foo = Atrib (Proxy :: Proxy "a") True       

foo' :: APEG ('("b", Char) ': '[]) ()
foo' = atrib (Proxy :: Proxy "b") 'a'        

mytest = Cat (Map (\ _ y -> y) (Cat (Map (\ _ y -> y) foo) foo')) (Get (Proxy :: Proxy "b"))        
mytest1 = (\_ _ c -> c) <$$> foo <**> foo' <**> get (Proxy :: Proxy "b")
