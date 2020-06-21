-- The objects with which the player can interact.
-- Remember to run ./regenPublicObjects.sh after modifying this file!
module Objects where

import Object


newtype Key = Key (Object ())

newtype Door = Door (Object DoorFields)
data DoorFields = DoorFields
  { doorLocked :: Bool
  }
