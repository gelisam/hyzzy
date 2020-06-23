{-# LANGUAGE OverloadedStrings #-}
module Rooms.B6 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."




east  :: Command
east  = Command $ goToRoom "B7"

south :: Command
south = Command $ goToRoom "C6"





--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |*  |
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
