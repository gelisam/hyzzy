{-# LANGUAGE GADTs #-}
module Hyzzy.Object where

import Data.IORef
import Data.Typeable
import Data.Unique


data Object fields = Object
  { objectId     :: Unique
  , objectFields :: IORef fields
  }

-- e.g. 'Foo' for 'newtype Foo = Foo (Object FooFields)'
type Ctor object fields
  = Object fields -> object

data Instance object where
  Instance :: Ctor object fields
           -> fields
           -> Instance object

data SomeInstance where
  SomeInstance :: Typeable object
               => Instance object
               -> SomeInstance
