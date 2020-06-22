{-# LANGUAGE OverloadedStrings #-}
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
