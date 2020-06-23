-- The state of the game at the beginning of the adventure.
{-# LANGUAGE OverloadedStrings #-}
module Start where

import Hyzzy.Command
import Hyzzy.Room


intro
  :: Command
intro = Command $ do
  display "Another toy text adventure where commands have Haskell types."
  display "Type \":help\" to view the meta-commands."

startingRoom
  :: RoomName
startingRoom
  = "D4"
