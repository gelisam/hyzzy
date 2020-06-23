{-# LANGUAGE OverloadedStrings #-}
module Rooms.E4 where

import Hyzzy.Command


look :: Command
look = Command $ do
  display "More foliage walls. This place is a maze..."
  display ""
  display "On the north is the paper-dispensing machine."

north :: Command
north = Command $ goToRoom "D4"




south :: Command
south = Command $ goToRoom "F4"

west  :: Command
west  = Command $ goToRoom "E3"


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
-- E| | |  *| |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
