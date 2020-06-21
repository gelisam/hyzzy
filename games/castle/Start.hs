-- The state of the game at the beginning of the adventure.
module Start where

import Command
import Objects


intro
  :: Command
intro = do
  display "A toy text adventure where commands have Haskell types."
  display "Type \":help\" to view the meta-commands."

  -- TODO: put the door in the environment, not in the inventory!
  addToInventory "door" Door $ DoorFields
    { doorLocked = True
    , doorOpened = False
    }

  addToInventory "key" Key ()
