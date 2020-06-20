{-# LANGUAGE OverloadedStrings #-}
module Commands where

import Command
import Objects


look :: Command
look = "You stand in front of the castle's heavy heavy door."

open :: Door -> Command
open _ = "It's locked."

unlock :: Key -> Door -> Command
unlock _ _ = "The door opens with a drawn-out yawn. You're in!"
