{-#LANGUAGE DeriveFunctor, 
            FlexibleInstances,
            ScopedTypeVariables,
            RankNTypes,
            GADTs #-}

module Text.APEG.Semantics.APEGSem where

import Prelude hiding ((>>=), (>>), return, fail)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad 
import Control.Monad.Reader

import GHC.TypeLits

import Text.APEG.Syntax.APEGSyn
                         
-- A simple parser library that keeps the PEG as internal state

data PState env = PState {
                   current  :: forall b. PEG env b
                ,  syntaxes :: forall b c env'. Map String (PEG env b -> PEG env' c)
                }

data Parser s env a = Parser { runParser :: s -> Reader (PState env) (Result s a) }

data Result s a
    = Pure a
    | Commit s a
    | Fail String Bool
      deriving (Functor, Show)

instance Functor (Parser s env) where
  fmap f p = Parser (\s ->
                      do
                        r <- runParser p s
                        case r of
                          Pure a -> return (Pure (f a))
                          Fail s b -> return (Fail s b)
                          Commit s a -> return (Commit s (f a)))


instance Applicative (Parser s env) where
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
                         
instance Alternative (Parser s env) where
    (Parser pf) <|> (Parser pa)
          = Parser $ \ s -> do
              rf <- pf s
              case rf of
                Fail _ False -> pa s
                x            -> return x
    empty = Parser $ \ _ -> return (Fail "empty" False)

instance Monad (Parser s env) where
    return = pure
    fail s = Parser $ \ _ -> return (Fail s False)
    (Parser m) >>= k = Parser $ \ s ->
                          do
                            r <- m s
                            case r of
                              Pure a -> runParser (k a) s
                              Fail s c -> return (Fail s c)
                              Commit s' a -> runParser (k a) s'

instance MonadPlus (Parser s env) where
    mplus = (<|>)
    mzero = empty

-- basic combinators
            
try :: Parser s env a -> Parser s env a
try (Parser p) = Parser $ \ d -> do
                     r <- p d
                     case r of
                       Fail s _ -> return (Fail s False)
                       x -> return x

infixl 3 </>
                           
(</>) :: Parser s env a -> Parser s env a -> Parser s env a
p </> q = try p <|> q                           


class Stream s where
   anyChar :: Parser s env Char

instance Stream String where
   anyChar = Parser $ \ s ->
              case s of
                (x:xs) -> return (Commit xs x)
                []     -> return (Fail "EOF" False)
                          
satsem :: Stream c => (Char -> Bool) -> Parser c env Char
satsem p = try $ do
           x <- anyChar
           x <$ guard (p x)     

char :: Stream c => Char -> Parser c env Char
char c = satsem (c ==)

         
string :: Stream c => String -> Parser c env String
string s = do
            s' <- replicateM (length s) anyChar
            s <$ guard (s == s')

-- semantics of parsing expressions

interpPEG :: Stream c => PEG env a -> Parser c env a
interpPEG (PEG s env) = interpPExp (lookupEnv s env)
              
interpPExp :: Stream c => PExp env a -> Parser c env a
interpPExp (Symb s) = interpSym s
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
interpPExp (Error s) = fail s

interpSym :: Stream c => Sym env a -> Parser c env a
interpSym (Term s) = string s
interpSym (NonTerm r)
  = Parser (\ s -> do
               e <- asks (lookupEnv r . exprs . current)
               runParser (interpPExp e) s)

-- modification API

insPExp :: forall env a . String -> PExp env a -> PState env -> PState (env , a)
insPExp s p ps = ps{syntaxes = Map.insert s p (syntaxes ps) }

insertPExp :: String -> PExp env a -> Parser s (env , a) ()
insertPExp s p = undefined

modifyPExp :: String -> PExp env a -> Parser s env ()
modifyPExp s p = undefined
