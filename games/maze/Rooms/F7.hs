{-# LANGUAGE OverloadedStrings #-}
module Rooms.F7 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = "More foliage walls. This place is a maze..."

note :: Instance Paper
note = Instance Paper $ PaperFields
  { paperLines = ["SOUTH"]
  }

north :: Command
north = Command $ goToRoom "E7"




south :: Command
south = Command $ goToRoom "G7"

west  :: Command
west  = Command $ goToRoom "F6"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |   |
--  + + +-+ +-+ + +
-- D|   |     | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |    *|
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
