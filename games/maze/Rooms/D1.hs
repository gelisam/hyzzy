{-# LANGUAGE OverloadedStrings #-}
module Rooms.D1 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = "More foliage walls. This place is a maze..."

note :: Instance Paper
note = Instance Paper $ PaperFields
  { paperLines = ["NORTH"]
  }

north :: Command
north = Command $ goToRoom "C1"

east  :: Command
east  = Command $ goToRoom "D2"

south :: Command
south = Command $ goToRoom "E1"




--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |   |
--  + + +-+ +-+ + +
-- D|*  |     | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
