{-# LANGUAGE OverloadedStrings #-}
module Rooms.A1 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = "More foliage walls. This place is a maze..."

note :: Instance Paper
note = Instance Paper $ PaperFields
  { paperLines = ["EAST"]
  }



east  :: Command
east  = Command $ goToRoom "A2"

south :: Command
south = Command $ goToRoom "B1"





--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|*  |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |   |
--  + + +-+ +-+ + +
-- D|   |     | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
