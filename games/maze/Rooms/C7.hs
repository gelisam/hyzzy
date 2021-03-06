{-# LANGUAGE OverloadedStrings #-}
module Rooms.C7 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."







south :: Command
south = Command $ goToRoom "D7"

west  :: Command
west  = Command $ goToRoom "C6"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |  *|
--  + + +-+ +-+ + +
-- D|   |     | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
