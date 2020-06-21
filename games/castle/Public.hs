-- The types of all the values transfered between the game and hyzzy.
module Public
  ( -- needed for the inventory.
    Char, Dynamic, Map

    -- needed for the game-specific commands.
  , Command

    -- needed for the game-specific objects.
    -- only export the type constructors, not the data constructors,
    -- or the player will be able to cheat!
  , Door, Key
  ) where

import Data.Dynamic
import Data.Map

import Command
import Commands
import Objects
