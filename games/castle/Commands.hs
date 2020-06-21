-- Commands which are available in every room.
{-# LANGUAGE MultiWayIf, OverloadedLabels, OverloadedStrings, RecordWildCards #-}
module Commands where

import Data.Generics.Labels ()

import Command
import Objects


look :: Command
look = "You stand in front of the castle's heavy heavy door."

open :: Door -> Command
open door = do
  DoorFields {..} <- getFields door
  if | doorLocked -> "It's locked."
     | doorOpened -> "It's already opened."
     | otherwise  -> do
       setField Door door #doorOpened True
       "The door opens with a drawn-out yawn. You're in!"

unlock :: Key -> Door -> Command
unlock key door = do
  consume Key key
  setField Door door #doorLocked False
  "That random key you happened to carry in your pockets happens to fit the lock. What are the odds?"
