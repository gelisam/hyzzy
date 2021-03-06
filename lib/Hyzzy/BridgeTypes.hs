-- The types of all the values transfered between the game and hyzzy.
module Hyzzy.BridgeTypes
  ( -- needed for the game-specific commands.
    Command, CommandF, Coyoneda, Free

    -- needed for the game-specific objects.
  , Object

    -- needed for the game-specific rooms.
  , RoomName
  ) where

import Control.Monad.Free
import Data.Functor.Coyoneda

import Hyzzy.Command
import Hyzzy.Object
import Hyzzy.Room
