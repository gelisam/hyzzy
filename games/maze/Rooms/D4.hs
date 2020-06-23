{-# LANGUAGE OverloadedStrings #-}
module Rooms.D4 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = Command $ do
  display "What have you gotten yourself into this time? You are at the center of a foliage"
  display "maze, with zero recollection of how you got here."
  display ""
  display "You stand next to a strange machine."


paperDispenser :: Instance PaperDispenser
paperDispenser = Instance PaperDispenser ()

activate :: PaperDispenser -> Command
activate _ = Command $ do
  display "Bzzt! A paper comes out. You take it."
  addToInventory "paper" Paper $ PaperFields
    { paperLines = []
    }

north :: Command
north = Command $ goToRoom "C4"

east  :: Command
east  = Command $ goToRoom "D5"

south :: Command
south = Command $ goToRoom "E4"

west  :: Command
west  = Command $ goToRoom "D3"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |   |
--  + + +-+ +-+ + +
-- D|   |  *  | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
