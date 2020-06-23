{-# LANGUAGE OverloadedStrings #-}
module Rooms.G4 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = Command $ do
  display "More foliage walls. This place is a maze..."
  display ""
  display "You can see the paper-dispensing machine further to the north."

note :: Instance Paper
note = Instance Paper $ PaperFields
  { paperLines = ["NORTH"]
  }

north :: Command
north = Command $ goToRoom "F4"

east  :: Command
east  = Command $ goToRoom "G5"




west  :: Command
west  = Command $ goToRoom "G3"


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
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|      *      |
--  +-+-+-+-+-+-+-+
