{-# LANGUAGE OverloadedStrings #-}
module Rooms.E3 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."

north :: Command
north = Command $ goToRoom "D3"

east  :: Command
east  = Command $ goToRoom "E4"








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
-- E| | |*  | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
