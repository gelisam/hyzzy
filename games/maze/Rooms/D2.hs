{-# LANGUAGE OverloadedStrings #-}
module Rooms.D2 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = "More foliage walls. This place is a maze..."

note :: Instance Paper
note = Instance Paper $ PaperFields
  { paperLines = ["WEST"]
  }

north :: Command
north = Command $ goToRoom "C2"




south :: Command
south = Command $ goToRoom "E2"

west  :: Command
west  = Command $ goToRoom "D1"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |   |
--  + + +-+ +-+ + +
-- D|  *|     | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
