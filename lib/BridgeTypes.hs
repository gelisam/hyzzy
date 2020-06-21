-- The types of all the values transfered between the game and hyzzy.
module BridgeTypes
  ( -- needed for the inventory.
    Char, Dynamic, Map

    -- needed for the game-specific commands.
  , Command, CommandF, Free
  ) where

import Control.Monad.Free
import Data.Dynamic
import Data.Map

import Command
