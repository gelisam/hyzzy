{-# LANGUAGE OverloadedStrings #-}
module Rooms.D5 where

import Hyzzy.Command


look :: Command
look = Command $ do
  display "More foliage walls. This place is a maze..."
  display ""
  display "On the west is the paper-dispensing machine."







south :: Command
south = Command $ goToRoom "E5"

west  :: Command
west  = Command $ goToRoom "D4"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |   |
--  + + +-+ +-+ + +
-- D|   |    *| | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
