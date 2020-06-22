-- Commands which are available in every room.
{-# LANGUAGE MultiWayIf, OverloadedLabels, OverloadedStrings, RecordWildCards #-}
module Commands where

import Data.Generics.Labels ()

import Hyzzy.Command

import Objects


open :: Door -> Command
open door = Command $ do
  DoorFields {..} <- getFields door
  if | doorLocked -> display "It's locked."
     | doorOpened -> display "It's already opened."
     | otherwise  -> do
       setField Door door #doorOpened True
       display "The door opens with a drawn-out yawn. You're in!"

unlock :: Key -> Door -> Command
unlock key door = Command $ do
  consume Key key
  setField Door door #doorLocked False
  display "That random key you happened to carry in your pockets happens to fit the lock. What are the odds?"
