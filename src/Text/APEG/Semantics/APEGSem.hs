{-#LANGUAGE DeriveFunctor, 
            FlexibleInstances, 
            GADTs, 
            TypeFamilies,
            UndecidableInstances,
            TypeOperators, 
            DataKinds,
            KindSignatures,
            ScopedTypeVariables,
            PolyKinds #-}

module Text.APEG.Semantics.APEGSem where

import Prelude hiding ((>>=), (>>), return, fail)

import Control.Applicative
import Control.Monad
import Control.Monad.State    
    
import Data.Proxy
import Data.Singletons.Decide    
import Data.Singletons.Prelude    
import Data.Singletons.Prelude.List
import Data.Type.Equality    
    
import GHC.Exts    
import GHC.TypeLits
    
import Text.APEG.Syntax.APEGSyn


-- attribute definition and functions
    
data Attr (xs :: [(Symbol,*)]) where
   Nil  :: Attr '[]
   (:*) :: (Sing s, t) -> Attr xs -> Attr ('(s , t) ': xs)
           
lookupAttr :: (Lookup s env ~ 'Just t) => Sing s -> Attr env -> t
lookupAttr s ((s',t) :* env')
    = case s %:== s' of
        STrue  -> t
        SFalse -> lookupAttr s env'          
                  
updateAttr :: (Lookup s env ~ 'Just t) => Sing s -> t -> Attr env -> Attr env
updateAttr s v ((s',v') :* env')
           = case s %:== s' of
               STrue  -> (s', v) :* env'
               SFalse -> (s', v') :* updateAttr s v env'

-- parser definition

newtype Parser s env a = Parser { runParser :: s -> State (Attr env) (Result s a) }
                     deriving Functor

data Result s a
    = Pure a
    | Commit s a
    | Fail String Bool
      deriving Functor


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
                          
sat :: Stream c => (Char -> Bool) -> Parser c env Char
sat p = try $ do
           x <- anyChar
           x <$ guard (p x)     

char :: Stream c => Char -> Parser c env Char
char c = sat (c ==)

         
string :: Stream c => String -> Parser c env String
string s = do
            s' <- replicateM (length s) anyChar
            s <$ guard (s == s')

-- semantics of parsing expressions
              
interp :: PExp env a -> Parser String env a
interp (Sat f) = sat f
interp (Symb s) = string s
interp (Success a) = pure a
interp (Map f p) = f <$> interp p
interp (Bind p f) = (interp p) >>= interp . f
interp (Failure s) = fail s
interp (Not p)
     = (try (interp p) *> empty) <|> pure ()
interp (Cat p q) = interp p <*> interp q
interp (Choice p q) = interp p </> interp q
interp (Star p) = many (interp p)
interp (Get s) = Parser $ \ _ -> gets (lookupAttr s) >>= return . Pure                  
interp (Set s v) = Parser $ \ _ ->
                        do
                           modify (updateAttr s v)
                           return (Pure ())       
interp (Check s p) = Parser $ \ _ ->
                        do
                          v <- gets (lookupAttr s)
                          if p v then return (Pure ())
                             else fail "attribute"             
