{-# LANGUAGE OverloadedStrings #-}
module Rooms.A3 where

import Hyzzy.Command


look :: Command
look = "More foliage wa... wait! Is this the exit??"

north :: Command
north = Command $ do
  display "You finally leave the maze! You are so happy to finally see a wall which is not"
  display "made of foliage. This wall is a real wall, made of large, heavy rocks, the kind of"
  display "wall which makes you wonder why you didn't simply walk through the foliage. Such a"
  display "nice, heavy wall. Nice door, too."
  display ""
  display "You stand in front of the castle's heavy door."
  display ""
  display "THE END"

east  :: Command
east  = Command $ goToRoom "A4"








--   1 2 3 4 5 6 7
--  +-+-+ +-+-+-+-+
-- A|   |*        |
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
