{-# LANGUAGE OverloadedStrings #-}
module Rooms.A5 where

import Hyzzy.Command


look :: Command
look = "More foliage walls. This place is a maze..."




east  :: Command
east  = Command $ goToRoom "A6"




west  :: Command
west  = Command $ goToRoom "A4"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |    *    |
--  + +-+-+-+-+-+ +
-- B| |       |   |
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
