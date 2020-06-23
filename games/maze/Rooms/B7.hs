{-# LANGUAGE OverloadedStrings #-}
module Rooms.B7 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."

north :: Command
north = Command $ goToRoom "A7"







west  :: Command
west  = Command $ goToRoom "B6"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |  *|
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
