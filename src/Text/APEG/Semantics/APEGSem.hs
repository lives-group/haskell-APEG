{-#LANGUAGE DeriveFunctor, 
            FlexibleInstances,
            ScopedTypeVariables,
            RankNTypes,
            DataKinds,
            TypeOperators,
            KindSignatures,
            RebindableSyntax,
            GADTs #-}

module Text.APEG.Semantics.APEGSem where

import Prelude hiding ((>>=), return)

import Control.Applicative
import Control.Monad hiding ((>>=), return)
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List

import GHC.TypeLits

import Text.APEG.Syntax.APEGSyn

-- simple indexed state monad

                         
-- A simple parser library that keeps the PEG as internal state

data Parser s a = Parser { runParser :: forall b xs. s -> State (Env xs) (Result s a) }

data Env (xs :: [Symbol]) where
  Empty :: Env '[]
  Ext   :: (Elem s xs ~ 'False,
            KnownSymbol s) => Sing s -> PExp env a -> Env xs -> Env (s ': xs)

data Result s a
    = Pure a
    | Commit s a
    | Fail String Bool
      deriving (Functor, Show)

instance Functor (Parser s) where
  fmap f p = Parser (\s ->
                      do
                        r <- runParser p s
                        case r of
                          Pure a -> return (Pure (f a))
                          Fail s b -> return (Fail s b)
                          Commit s a -> return (Commit s (f a)))


instance Applicative (Parser s) where
    pure v = Parser $ \ _ -> return (Pure v)
    (Parser pf) <*> (Parser pa)
                 = Parser $ \ s ->
                            do
                              rf <- pf s
                              case rf of
                                Pure f -> do
                                        ra <- pa s
                                        case ra of
                                          Pure a -> return (Pure (f a))
                                          Fail s' _ -> return (Fail s' True)
                                          Commit d' a -> return (Commit d' (f a))
                                Fail s b -> return (Fail s b)
                                Commit d f -> do
                                        ra <- pa d
                                        case ra of
                                          Pure a -> return (Commit d (f a))
                                          Fail s' _ -> return (Fail s' True)
                                          Commit d' a -> return (Commit d' (f a))
                         
instance Alternative (Parser s) where
    (Parser pf) <|> (Parser pa)
          = Parser $ \ s -> do
              rf <- pf s
              case rf of
                Fail _ False -> pa s
                x            -> return x
    empty = Parser $ \ _ -> return (Fail "empty" False)

instance Monad (Parser s) where
    return = pure
    fail s = Parser $ \ _ -> return (Fail s False)
    (Parser m) >>= k = Parser $ \ s ->
                          do
                            r <- m s
                            case r of
                              Pure a -> runParser (k a) s
                              Fail s c -> return (Fail s c)
                              Commit s' a -> runParser (k a) s'

instance MonadPlus (Parser s) where
    mplus = (<|>)
    mzero = empty

-- basic combinators
            
try :: Parser s a -> Parser s a
try (Parser p) = Parser $ \ d -> do
                     r <- p d
                     case r of
                       Fail s _ -> return (Fail s False)
                       x -> return x

infixl 3 </>
                           
(</>) :: Parser s a -> Parser s a -> Parser s a
p </> q = try p <|> q                           


class Stream s where
   anyChar :: Parser s Char

instance Stream String where
   anyChar = Parser $ \ s ->
              case s of
                (x:xs) -> return (Commit xs x)
                []     -> return (Fail "EOF" False)
                          
satsem :: Stream c => (Char -> Bool) -> Parser c Char
satsem p = try $ do
           x <- anyChar
           x <$ guard (p x)     

char :: Stream c => Char -> Parser c Char
char c = satsem (c ==)

         
string :: Stream c => String -> Parser c String
string s = do
            s' <- replicateM (length s) anyChar
            s <$ guard (s == s')

-- semantics of parsing expressions

interpPEG :: Stream c => PEG env a -> Parser c a
interpPEG (PEG s env) = undefined
              
interpPExp :: Stream c => PExp env a -> Parser c a
interpPExp (Term s) = string s
interpPExp (NonTerm s) = undefined 
interpPExp (Cat e e') = interpPExp e <*> interpPExp e'
interpPExp (Choice e e') = interpPExp e <|> interpPExp e'
interpPExp (Neg e) =  ((try (interpPExp e)) *> fail "not") <|> return ()
interpPExp (Star e) = many (interpPExp e)
interpPExp (Map f e) = f <$> interpPExp e
interpPExp (Bind f e)
  = do
     v <- interpPExp f
     interpPExp (e v)
interpPExp (Success v) = return v
interpPExp (Failure s) = fail s


-- parser modification API

insertNT :: (Stream c, KnownSymbol s) => Sing s -> PExp env a -> Parser c ()
insertNT s p
  = Parser (\_ -> modify (Ext s p) >> return (Pure ()))             
               
