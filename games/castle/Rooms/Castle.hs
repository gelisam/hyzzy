{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Rooms.Castle where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = "The castle's opulence is even more exhuberant than the rumors claimed!"

door :: Instance Door
door = Instance Door $ DoorFields
  { doorLocked = False
  , doorOpened = True
  }

treasure :: Instance Treasure
treasure = Instance Treasure ()

take :: Treasure -> Command
take treasure = Command $ do
  treasureFields <- getFields treasure
  consume Treasure treasure
  addToInventory "treasure" Treasure treasureFields
  display "Once you sell this at the town, you'll finally be able to buy that adamantium armor you've always wanted."

exit :: Door -> Command
exit _ = Command $ do
  goToRoom "Garden"
