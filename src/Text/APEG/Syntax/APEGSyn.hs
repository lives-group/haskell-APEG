{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE PolyKinds                 #-}

module Text.APEG.Syntax.APEGSyn where

import Control.Applicative

import GHC.TypeLits

-- membership proof building

type family HasSymbol (g :: [(Symbol,*)]) (s :: Symbol) :: Maybe * where
  HasSymbol '[]            s = 'Nothing
  HasSymbol ('(s, a) ': g) s = 'Just a
  HasSymbol ('(t, a) ': g) s = HasSymbol g s

-- proxies for names

data Name (s :: Symbol) = Var
data Type (a :: *) = Of

-- definition of a variable

data ScopedSymbol (g :: [(Symbol,*)]) (a :: *)
  = forall s.
      (HasSymbol g s ~ 'Just a) => The (Name s)

-- definition of parsing expressions

data PExp (g :: [(Symbol,*)]) (a :: *) where
  ESymb :: String -> PExp g String
  EVar :: ScopedSymbol g a -> PExp g a
  ECat  :: PExp g (a -> b) -> PExp g a -> PExp g b
  EChoice :: PExp g a -> PExp g a -> PExp g a
  ENeg :: PExp g a -> PExp g ()
  EStar :: PExp g a -> PExp g [a]
  EMap :: (a -> b) -> PExp g a -> PExp g b
  EPure :: a -> PExp g a
  EFail :: PExp g a
  EBind :: PExp g a -> (a -> PExp g b) -> PExp g b
    
-- instances for PExp

instance Functor (PExp g) where
  fmap = EMap

instance Applicative (PExp g) where
  pure = EPure
  (<*>) = ECat

instance Alternative (PExp g) where
  empty = EFail
  (<|>) = EChoice

instance Monad (PExp g) where
  return = EPure
  (>>=) = EBind
  fail _ = EFail


-- definition of adaptable parsing expressions

infixr 5 :>

data APExp (g :: [(Symbol,*)])(h :: [(Symbol,*)])(a :: *) where
  ADone :: APExp g g a -- ending a composite expression
  APExp :: PExp g a -> APExp g g a -- just a pexp
  ARule :: HasSymbol g s ~ 'Just a  => -- add a rule in a specific non terminal.
           Name s -> PExp g a -> APExp g g ()  
  ASymb :: HasSymbol g s ~ 'Nothing =>   -- add a new non-terminal
           Name s -> Type a -> PExp g a -> APExp g ( '(s,a) ': g) a
  (:>)  :: APExp g h (a -> b) -> APExp h i a -> APExp g i b -- composition


-- a grammar is just is starting rule

data APEG (s :: Symbol) (a :: *)
    = forall h. HasSymbol h s ~ 'Just a =>
                 APEG { start :: Name s
                      , prods :: [ (Symbol, APExp '[] h a) ]
                      }
