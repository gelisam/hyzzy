{-# LANGUAGE OverloadedStrings #-}
module Start where

import Data.Dynamic
import qualified Data.Map as Map

import Command
import Inventory
import Objects


intro
  :: Command
intro = do
  "A toy text adventure where commands have Haskell types.\n\
  \Type \":help\" to view the meta-commands."


initialInventory
  :: Inventory
initialInventory
  = Map.fromList
    [ ("door", toDyn Door)  -- TODO: put the door in the environment, not in the inventory!
    , ("key", toDyn Key)
    ]
