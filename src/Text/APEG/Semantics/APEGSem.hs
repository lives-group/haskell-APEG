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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List

import GHC.TypeLits

import Text.APEG.Syntax.APEGSyn
import Text.APEG.Semantics.Parser


compile :: Stream s => PExp env tys a -> Parser s (Zip env tys) (Zip env tys) a
compile (Term s)
  = string s
compile (NonTerm s)
  = do
      env <- iget
      compile (sLookup s env)
                          

