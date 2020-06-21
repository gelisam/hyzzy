-- The types of all the values transfered between the game and hyzzy.
module Hyzzy.BridgeTypes
  ( -- needed for the inventory.
    Char, Dynamic, Map

    -- needed for the game-specific commands.
  , Command, CommandF, Coyoneda, Free

    -- needed for the game-specific objects.
  , Object
  ) where

import Control.Monad.Free
import Data.Dynamic
import Data.Functor.Coyoneda
import Data.Map

import Hyzzy.Command
import Hyzzy.Object
