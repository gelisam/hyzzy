{-# LANGUAGE OverloadedStrings #-}
module Rooms.E7 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."

north :: Command
north = Command $ goToRoom "D7"




south :: Command
south = Command $ goToRoom "F7"

west  :: Command
west  = Command $ goToRoom "E6"


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
-- E| | |   | |  *|
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
