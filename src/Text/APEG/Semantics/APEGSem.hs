{-#LANGUAGE DeriveFunctor #-}

module Text.APEG.Semantics.APEGSem where


import Control.Applicative
import Control.Monad


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
