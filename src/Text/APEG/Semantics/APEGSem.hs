{-#LANGUAGE DeriveFunctor, 
            FlexibleInstances,
            ScopedTypeVariables,
            RankNTypes,
            DataKinds,
            PolyKinds,
            TypeOperators,
            KindSignatures,
            RebindableSyntax,
            GADTs #-}

module Text.APEG.Semantics.APEGSem where

import Prelude hiding ((>>=), return)

import Control.Applicative
import Control.Monad hiding ((>>=), return)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List

import GHC.TypeLits

import Text.APEG.Syntax.APEGSyn
import Text.APEG.Semantics.Parser

{-

data Ex4 (p :: k -> k' -> k'' -> k''' ->  *) where
  Ex4 :: p a b c d -> Ex4 p

compile :: Ex2 PExp -> Ex4 Parser
compile (Ex2 (Term s))
  = Ex4 (string s)
compile (Ex2 (NonTerm s))
  = do
      env <- iget
      compile (lookupEnv s env)
        
-}
