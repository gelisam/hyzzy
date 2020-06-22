-- The objects with which the player can interact.
-- Remember to run "./regenPublicObjects.sh games/castle" after modifying this file!
{-# LANGUAGE DeriveGeneric #-}
module Objects where

import GHC.Generics

import Hyzzy.Object


newtype Key = Key (Object ())

newtype Door = Door (Object DoorFields)
data DoorFields = DoorFields
  { doorLocked :: Bool
  , doorOpened :: Bool
  }
  deriving Generic

newtype Treasure = Treasure (Object ())
