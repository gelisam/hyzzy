{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables, TypeApplications, TypeSynonymInstances #-}
module Hyzzy.Command where

import Control.Lens
import Control.Monad.Free
import Data.Coerce
import Data.Functor.Coyoneda
import Data.String
import Data.Typeable

import Hyzzy.Object



data CommandF r where
  Display        :: String -> CommandF ()
  AddToInventory :: Typeable object
                 => String
                 -> Ctor object fields
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
  -> Ctor object fields
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
  => Ctor object fields
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
  => Ctor object fields
  -> object
  -> Command
consume _ object
  = liftF $ liftCoyoneda
  $ Consume (coerce @object @(Object fields) object)

instance IsString Command where
  fromString = display
