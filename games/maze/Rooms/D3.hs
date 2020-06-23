{-# LANGUAGE OverloadedStrings #-}
module Rooms.D3 where

import Hyzzy.Command


look :: Command
look = Command $ do
  display "More foliage walls. This place is a maze..."
  display ""
  display "On the east is the paper-dispensing machine."




east  :: Command
east  = Command $ goToRoom "D4"

south :: Command
south = Command $ goToRoom "E3"





--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |     |   |
--  + + +-+ +-+ + +
-- D|   |*    | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
