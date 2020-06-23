{-# LANGUAGE OverloadedStrings #-}
module Rooms.D7 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."

north :: Command
north = Command $ goToRoom "C7"




south :: Command
south = Command $ goToRoom "E7"





--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |   |
--  + + +-+ +-+ + +
-- D|   |     | |*|
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
