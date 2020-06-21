{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs, TypeSynonymInstances #-}
module Command where

import Control.Monad.Free
import Data.Functor.Coyoneda
import Data.String
import Data.Typeable

import Object



data CommandF r where
  Display        :: String -> CommandF ()
  AddToInventory :: Typeable object
                 => String -> (Object fields -> object) -> fields -> CommandF ()

type Command = Free (Coyoneda CommandF) ()

display
  :: String -> Command
display s
  = liftF $ liftCoyoneda $ Display s

addToInventory
  :: Typeable object
  => String
  -> (Object fields -> object)  -- e.g. 'Foo' for 'newtype Foo = Foo (Object FooFields)'
  -> fields
  -> Command
addToInventory name mkObject fields
  = liftF $ liftCoyoneda $ AddToInventory name mkObject fields

instance IsString Command where
  fromString = display
