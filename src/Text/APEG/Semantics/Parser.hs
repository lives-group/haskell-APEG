{-# LANGUAGE FlexibleInstances,
             KindSignatures,
             PolyKinds, 
             DataKinds,
             RebindableSyntax,
             DeriveFunctor #-}

module Text.APEG.Semantics.Parser where

import Prelude hiding (return, fmap, (>>=), (>>), guard,pure, (<$), (<*>))

import Data.Singletons.Prelude

import Text.APEG.Utils.Indexed

-- definition of parsing results

data Result s a
    = Pure a
    | Commit s a
    | Fail String  Bool
    deriving (Functor, Show)

-- a type class for parsing inputs

class Stream s where
   anyChar :: Parser s env env Char

-- strings are an instance for parsing

instance Stream String where
   anyChar = Parser  $ \ s env ->
              case s of
                (x:xs) -> Commit xs (x, env)
                []     -> Fail "EOF"  False

-- parser data type                

newtype Parser (s :: *) (env :: [(Symbol,*)]) (env' :: [(Symbol,*)]) (a :: *)
        = Parser { runParser :: s -> Sing env -> Result s (a, Sing env') }

-- some parsing instances

instance IFunctor (Parser s) where
  imap f p = Parser (\ s env ->
                      let
                        r = runParser p s env
                      in case r of
                            Pure (a,env') -> Pure ((f a), env')
                            Fail s' b -> Fail s' b
                            Commit s' (a,env') -> Commit s' ((f a), env'))

instance IPointed (Parser s) where
  ireturn x = Parser (\ _ env -> Pure (x, env))

instance IApplicative (Parser s) where
  iap p p' = Parser (\ s env ->
                      case runParser p s env of
                           Pure (f,env') ->
                             case runParser p' s env' of
                                  Pure (a,env'') -> Pure (f a, env'')
                                  Fail s' b -> Fail s' b
                                  Commit s' (a,env'') -> Commit s' (f a, env'')
                           Fail s' b -> Fail s' b
                           Commit s' (f,env') ->
                             case runParser p' s env' of
                                  Pure (a,env'') -> Pure (f a, env'')
                                  Fail s' b -> Fail s' b
                                  Commit s'' (a,env'') -> Commit s'' (f a, env''))
                                  

instance IMonad (Parser s) where
  ibind m k = Parser (\s env ->
                       case runParser m s env of
                            Pure (a,env') ->
                              runParser (k a) s env'
                            Commit s' (a,env') ->
                              runParser (k a) s' env'
                            Fail s' b -> Fail s' b)


-- preparing for rebindable syntax

return :: a -> Parser s env env a
return = ireturn

pure :: a -> Parser s env env a
pure = ireturn

(>>=) :: Parser s env env' a -> (a -> Parser s env' env'' b) -> Parser s env env'' b
(>>=) = ibind

(<*>) :: Parser s env env' (a -> b) -> Parser s env' env'' a -> Parser s env env'' b
(<*>) = iap

(<$>) :: (a -> b) -> Parser s env env' a -> Parser s env env' b
(<$>) = imap

(<|>) :: Parser s env env' a -> Parser s env env' a -> Parser s env env' a
p <|> q = Parser (\ s env ->
                   case runParser p s env of
                     Fail _ False -> runParser q s env
                     x            -> x)

empty :: Parser s env env a
empty = Parser (\ _ _ -> Fail "empty" False)

guard :: Bool -> Parser s env env ()
guard True = pure ()
guard False = empty

(<$) :: a -> Parser s env env' b -> Parser s env env' a
(<$) = imap . const

replicateM :: Int -> Parser s env env a -> Parser s env env [a]
replicateM 0 _ = ireturn []
replicateM n m
  = do
      x <- m
      xs <- replicateM (n - 1) m
      return (x : xs)

-- some simple parsers

try :: Parser s env env' a -> Parser s env env' a
try p = Parser (\s env ->
                 case runParser p s env of
                   Fail s _ -> Fail s False
                   x -> x)

satsem :: Stream c => (Char -> Bool) -> Parser c env env Char
satsem p = try $ do
           x <- anyChar
           x <$ guard (p x)     

char :: Stream c => Char -> Parser c env env Char
char c = satsem (c ==)

         
string :: Stream c => String -> Parser c env env String
string s = do
            s' <- replicateM (length s) anyChar
            s <$ guard (s == s')

-- monadic state interface

iget :: Parser s env env (Sing env)
iget = Parser (\ _ env -> Pure (env,env))

iput :: Sing env' -> Parser s env env' ()
iput env' = Parser (\_ _ -> Pure ((), env'))
