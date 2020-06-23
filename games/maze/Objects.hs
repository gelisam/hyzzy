-- The objects with which the player can interact.
-- Remember to run "./regenPublicObjects.sh games/castle" after modifying this file!
{-# LANGUAGE DeriveGeneric #-}
module Objects where

import GHC.Generics

import Hyzzy.Object


newtype PaperDispenser = PaperDispenser (Object ())

newtype Paper = Paper (Object PaperFields)
data PaperFields = PaperFields
  { paperLines :: [String]
  }
  deriving Generic

newtype Pen = Pen (Object ())
