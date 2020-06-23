{-# LANGUAGE OverloadedStrings #-}
module Rooms.C4 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = Command $ do
  display "More foliage walls. This place is a maze..."
  display ""
  display "On the south is the paper-dispensing machine."

note :: Instance Paper
note = Instance Paper $ PaperFields
  { paperLines = ["EAST"]
  }






east  :: Command
east  = Command $ goToRoom "C5"

south :: Command
south = Command $ goToRoom "D4"

west  :: Command
west  = Command $ goToRoom "C3"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |  *  |   |
--  + + +-+ +-+ + +
-- D|   |     | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
