-- The objects with which the player can interact.
-- Remember to also add them to PublicObjects.hs!
module Objects where

import Object


newtype Key = Key (Object ())

newtype Door = Door (Object DoorFields)
data DoorFields = DoorFields
  { doorLocked :: Bool
  }
