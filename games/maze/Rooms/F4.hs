{-# LANGUAGE OverloadedStrings #-}
module Rooms.F4 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = Command $ do
  display "More foliage walls. This place is a maze..."
  display ""
  display "The paper-dispensing machine is few steps to the north."

note :: Instance Paper
note = Instance Paper $ PaperFields
  { paperLines = ["WEST"]
  }

north :: Command
north = Command $ goToRoom "E4"




south :: Command
south = Command $ goToRoom "G4"

west  :: Command
west  = Command $ goToRoom "F3"


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
-- F| |    *|     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
