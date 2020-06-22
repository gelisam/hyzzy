-- The state of the game at the beginning of the adventure.
{-# LANGUAGE OverloadedStrings #-}
module Start where

import Hyzzy.Command
import Hyzzy.Room

import Objects


intro
  :: Command
intro = do
  display "A toy text adventure where commands have Haskell types."
  display "Type \":help\" to view the meta-commands."

  addToInventory "key" Key ()

startingRoom
  :: RoomName
startingRoom
  = "Garden"
