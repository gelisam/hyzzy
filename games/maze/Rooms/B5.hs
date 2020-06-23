{-# LANGUAGE OverloadedStrings #-}
module Rooms.B5 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."







south :: Command
south = Command $ goToRoom "C5"

west  :: Command
west  = Command $ goToRoom "B4"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |      *|   |
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
