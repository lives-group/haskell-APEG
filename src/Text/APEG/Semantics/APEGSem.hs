{-#LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Text.APEG.Semantics.APEGSem where


import Control.Applicative
import Control.Monad

import Text.APEG.Syntax.APEGSyn
    
newtype Parser s a = Parser { runParser :: s -> Result s a }
                     deriving Functor

data Result s a =
    Pure a            
  | Commit s a
  | Fail String Bool  
  deriving Functor
           
instance Applicative (Parser s) where
    pure x = Parser $ \ _ -> Pure x
    Parser pa <*> Parser pb
        = Parser $ \ s ->
               case pa s of
                 Pure f   -> f <$> pb s
                 Fail s b -> Fail s b
                 Commit d f ->
                     case pb s of
                       Pure a -> Commit d (f a)
                       Fail s'' _ -> Fail s'' True
                       Commit d'' a -> Commit d'' (f a)
           
instance Alternative (Parser s) where
    (Parser pf) <|> (Parser pa)
        = Parser $ \s ->
              case pf s of
                Fail _ False -> pa s
                x            -> x
    empty = Parser $ \ _ -> Fail "empty" False

instance Monad (Parser s) where
    return = pure
    (Parser m) >>= k = Parser $ \ d -> case m d of
                                         Pure a -> runParser (k a) d
                                         Commit d' a ->
                                             case runParser (k a) d' of
                                               Pure b -> Commit d' b
                                               Fail s _ -> Fail s True
                                               comm     -> comm
                                         Fail s c -> Fail s c
    fail s = Parser $ \ _ -> Fail s False

instance MonadPlus (Parser s) where
    mplus = (<|>)
    mzero = empty

try :: Parser s a -> Parser s a
try (Parser p) = Parser $ \ d ->
                    case p d of
                      Fail s _ -> Fail s False
                      x -> x

infixl 3 </>
                           
(</>) :: Parser s a -> Parser s a -> Parser s a
p </> q = try p <|> q                           


class Stream s where
    anyChar :: Parser s Char

instance Stream String where
    anyChar = Parser $ \s ->
              case s of
                (x:xs) -> Commit xs x
                []     -> Fail "EOF" False
                          
