{-# LANGUAGE OverloadedStrings #-}
module Rooms.C3 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = "More foliage walls. This place is a maze..."

map :: Instance Paper
map = Instance Paper $ PaperFields
  { paperLines =
    [ "  1 2 3 4 5 6 7 "
    , " +-+-+ +-+-+-+-+"
    , "A|   |         |"
    , " + +-+-+-+-+-+ +"
    , "B| |       |   |"
    , " + + +-+-+ + +-+"
    , "C| | |     |   |"
    , " + + +-+ +-+ + +"
    , "D|   |     | | |"
    , " + + + + + + + +"
    , "E| | |   | |   |"
    , " + + +-+ + +-+ +"
    , "F| |     |     |"
    , " + +-+-+ +-+-+ +"
    , "G|             |"
    , " +-+-+-+-+-+-+-+"
    ]
  }



east  :: Command
east  = Command $ goToRoom "C4"








--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |         |
--  + +-+-+-+-+-+ +
-- B| |       |   |
--  + + +-+-+ + +-+
-- C| | |*    |   |
--  + + +-+ +-+ + +
-- D|   |     | | |
--  + + + + + + + +
-- E| | |   | |   |
--  + + +-+ + +-+ +
-- F| |     |     |
--  + +-+-+ +-+-+ +
-- G|             |
--  +-+-+-+-+-+-+-+
