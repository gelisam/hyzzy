{-# LANGUAGE OverloadedStrings #-}
module Rooms.G7 where

import Hyzzy.Command



look :: Command
look = "More foliage walls. This place is a maze..."

north :: Command
north = Command $ goToRoom "F7"







west  :: Command
west  = Command $ goToRoom "G6"


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
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|            *|
--  +-+-+-+-+-+-+-+
