{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Rooms.Garden where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = "You stand in front of the castle's heavy door."

door :: Instance Door
door = Instance Door $ DoorFields
  { doorLocked = True
  , doorOpened = False
  }

enter :: Door -> Command
enter door = Command $ do
  DoorFields {..} <- getFields door
  if doorOpened
    then do
      display "You're in!"
      goToRoom "Castle"
    else do
      display "The door is closed."

leave :: Treasure -> Command
leave _ = "You leave the castle with a smile on your face, THE END. Type \":quit\" to leave the game as well."
