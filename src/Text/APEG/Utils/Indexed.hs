{-#LANGUAGE PolyKinds,
            DataKinds,
            FlexibleInstances #-}

module Text.APEG.Utils.Indexed where


-- some classes of indexed stuff


class IFunctor f where
  imap :: (a -> b) -> f x y a -> f x y b

class IFunctor f => IPointed f where
  ireturn :: a -> f x x a

class IPointed f => IApplicative f where
  iap :: f x y (a -> b) -> f y z a -> f x z b

class IApplicative f => IMonad f where
  ibind :: f x y a -> (a -> f y z b) -> f x z b
