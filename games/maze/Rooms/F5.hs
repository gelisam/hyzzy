{-# LANGUAGE OverloadedStrings #-}
module Rooms.F5 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."

north :: Command
north = Command $ goToRoom "E5"

east  :: Command
east  = Command $ goToRoom "F6"








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
-- F| |     |*    |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
