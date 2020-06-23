{-# LANGUAGE OverloadedStrings #-}
module Rooms.F6 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."




east  :: Command
east  = Command $ goToRoom "F7"




west  :: Command
west  = Command $ goToRoom "F5"


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
-- F| |     |  *  |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
