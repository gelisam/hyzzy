{-# LANGUAGE OverloadedStrings #-}
module Rooms.E5 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."

north :: Command
north = Command $ goToRoom "D5"




south :: Command
south = Command $ goToRoom "F5"





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
-- E| | |   |*|   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
