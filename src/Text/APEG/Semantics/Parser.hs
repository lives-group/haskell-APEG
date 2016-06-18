{-# LANGUAGE FlexibleInstances,
             KindSignatures,
             PolyKinds, 
             DataKinds,
             RebindableSyntax,
             DeriveFunctor #-}

module Text.APEG.Semantics.Parser where

import Prelude hiding (return, fmap, (>>=), (>>))

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
