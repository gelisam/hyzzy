-- Commands which are available in every room.
{-# LANGUAGE MultiWayIf, OverloadedLabels, OverloadedStrings, RecordWildCards #-}
module Commands where

import Data.Generics.Labels ()
import Data.Foldable

import Hyzzy.Command

import Objects


examine :: Paper -> Command
examine paper = Command $ do
  PaperFields {..} <- getFields paper
  if null paperLines
    then do
      display "It's a blank piece of paper."
    else do
      display "The note says:"
      for_ paperLines $ \paperLine -> do
        display (show paperLine)

write :: Pen -> Paper -> String -> Command
write _ paper paperLine = Command $ do
  PaperFields {..} <- getFields paper
  setField Paper paper #paperLines (paperLines ++ [paperLine])

pickUp :: Paper -> Command
pickUp paper = Command $ do
  paperFields <- getFields paper
  consume Paper paper
  addToInventory "paper" Paper paperFields

litter :: Paper -> Command
litter paper = Command $ do
  paperFields <- getFields paper
  consume Paper paper
  addToRoom "trash" Paper paperFields
