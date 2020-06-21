module Hyzzy.Object where

import Data.IORef
import Data.Unique


data Object fields = Object
  { objectId     :: Unique
  , objectFields :: IORef fields
  }

-- e.g. 'Foo' for 'newtype Foo = Foo (Object FooFields)'
type Ctor object fields
  = Object fields -> object
