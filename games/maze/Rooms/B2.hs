{-# LANGUAGE OverloadedStrings #-}
module Rooms.B2 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."




east  :: Command
east  = Command $ goToRoom "B3"

south :: Command
south = Command $ goToRoom "C2"





--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |*      |   |
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
