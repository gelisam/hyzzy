{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables, TypeApplications, TypeSynonymInstances #-}
module Command where

import Control.Lens
import Control.Monad.Free
import Data.Coerce
import Data.Functor.Coyoneda
import Data.String
import Data.Typeable

import Object



data CommandF r where
  Display        :: String -> CommandF ()
  AddToInventory :: Typeable object
                 => String
                 -> (Object fields -> object)
                 -> fields
                 -> CommandF ()
  GetFields      :: Object fields
                 -> CommandF fields
  SetField       :: Object fields
                 -> Lens' fields field
                 -> field
                 -> CommandF ()
  Consume        :: Object fields
                 -> CommandF ()

type Command = Free (Coyoneda CommandF) ()

display
  :: String -> Command
display s
  = liftF $ liftCoyoneda
  $ Display s

addToInventory
  :: Typeable object
  => String
  -> (Object fields -> object)  -- e.g. 'Foo' for 'newtype Foo = Foo (Object FooFields)'
  -> fields
  -> Command
addToInventory name mkObject fields
  = liftF $ liftCoyoneda
  $ AddToInventory name mkObject fields

getFields
  :: Coercible object (Object fields)
  => object
  -> Free (Coyoneda CommandF) fields
getFields object
  = liftF $ liftCoyoneda
  $ GetFields (coerce object)

setField
  :: Coercible object (Object fields)
  => (Object fields -> object)  -- e.g. 'Foo' for 'newtype Foo = Foo (Object FooFields)'
  -> object
  -> Lens' fields field
  -> field
  -> Command
setField _ object field value
  = liftF $ liftCoyoneda
  $ SetField (coerce object) field value

consume
  :: forall object fields
   . Coercible object (Object fields)
  => (Object fields -> object)  -- e.g. 'Foo' for 'newtype Foo = Foo (Object FooFields)'
  -> object
  -> Command
consume _ object
  = liftF $ liftCoyoneda
  $ Consume (coerce @object @(Object fields) object)

instance IsString Command where
  fromString = display
