module Hyzzy.Room where

import Data.String


newtype RoomName = RoomName
  { unRoomName :: String }
  deriving (Eq, Ord, Show)

instance IsString RoomName where
  fromString = RoomName
