module Object where

import Data.IORef
import Data.Unique


data Object fields = Object
  { objectId     :: Unique
  , objectFields :: IORef fields
  }
