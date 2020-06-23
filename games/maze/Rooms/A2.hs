{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Rooms.A2 where

import Hyzzy.Command
import Hyzzy.Object

import Objects


look :: Command
look = "More foliage walls. This place is a maze..."

note :: Instance Paper
note = Instance Paper $ PaperFields
  { paperLines = ["Thanks for obeying the signs! Enjoy this free pen."]
  }

take :: Pen -> Command
take pen = Command $ do
  penFields <- getFields pen
  consume Pen pen
  addToInventory "pen" Pen penFields

pen :: Instance Pen
pen = Instance Pen ()






west  :: Command
west  = Command $ goToRoom "A1"


--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|  *|         |
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
