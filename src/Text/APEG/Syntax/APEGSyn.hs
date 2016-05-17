{-# LANGUAGE GADTs, DataKinds, PolyKinds, 
             TypeOperators, TypeFamilies, 
             ScopedTypeVariables, 
             ConstraintKinds, UndecidableInstances #-}

module Text.APEG.Syntax.APEGSyn where

import Control.Applicative
    
import Data.Char    
import Data.Proxy    
import Data.Singletons.Prelude   
import Data.Singletons.Prelude.List

import GHC.Exts    
import GHC.TypeLits

type family In (s :: Symbol)(a :: *)(env :: [(Symbol, *)]) :: Constraint where
    In x t '[] = ()
    In x t ('(y,t) ': env) = (x ~ y , In x t env)

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
    Set :: (KnownSymbol s, In s t env) => proxy s -> t -> PExp env ()
    Get :: (KnownSymbol s, In s t env) => proxy s -> PExp env t
    Check :: (KnownSymbol s, In s t env) => proxy s -> (t -> Bool) -> PExp env Bool

data APeg (env :: [(Symbol, *)]) (a :: *) where
    APeg :: PExp env a -> APeg env a

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
    
            
-- type family At (k :: Symbol) (xs :: [(Symbol, a)]) where
--     At x ('(x,y) ': xs) = y
--     At x ('(z,y) ': xs) = At x xs
  
-- data APEG (env :: [(Symbol,*)]) (a :: *) where
--   Sat :: (Char -> Bool) -> APEG env Char
--   Symb :: String -> APEG env String        
--   Success :: a -> APEG env a       
--   Failure :: APEG env a
--   Not :: APEG env a -> APEG env ()
--   Cat :: APEG env (a -> b) -> APEG env' a -> APEG (env :++ env') b
--   Choice :: APEG env a -> APEG env a -> APEG env a
--   Star :: APEG env a -> APEG env [a]
--   Map :: (a -> b) -> APEG env a -> APEG env b
--   Atrib :: KnownSymbol s => proxy s -> b -> APEG ('(s,b) ': env) ()
--   Get :: KnownSymbol s => proxy s -> APEG env (s `At` env)       
            

-- sat :: (Char -> Bool) -> APEG env Char
-- sat = Sat       
         
-- neg :: APEG env a -> APEG env ()
-- neg = Not

-- star :: APEG env a -> APEG env [a]
-- star p = Star p

-- string :: String -> APEG env String
-- string = Symb          

-- atrib :: KnownSymbol s => proxy s -> b -> APEG ('(s,b) ': env) ()
-- atrib = Atrib         

-- get :: KnownSymbol s => proxy s -> APEG env (s `At` env)
-- get = Get

-- check :: KnownSymbol s => proxy s -> ((s `At` env) -> Bool) -> APEG env Bool
-- check s f = f <$$> Get s         

-- infixl 4 <$$>
-- infixl 4 <**>   
-- infixl 3 </>    
            
-- (<$$>) :: (a -> b) -> APEG env a -> APEG env b
-- f <$$> p = Map f p          

-- (<**>) :: APEG env (a -> b) -> APEG env' a -> APEG (env :++ env') b
-- p <**> q = Cat p q
          
-- (</>) :: APEG env a -> APEG env a -> APEG env a
-- p </> q = Choice p q          
           
-- -- simple tests
            
-- foo :: APEG ('("a", Bool) ': '[]) ()
-- foo = Atrib (Proxy :: Proxy "a") True       

-- foo' :: APEG ('("b", Char) ': '[]) ()
-- foo' = atrib (Proxy :: Proxy "b") 'a'        

-- mytest1 = ((\_ _ c -> c) <$$> foo <**> foo' <**> get (Proxy :: Proxy "b"))

-- -- more tests

-- digit :: APEG env Char
-- digit = sat isDigit         

-- number :: APEG env Int
-- number = f <$$> star digit
--          where
--            f = foldl (\a b -> a * 10 + b) 0 . map g
--            g c = ord c - ord '0'
