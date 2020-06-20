module Command where

import Data.String


newtype Command = Command
  { runCommand :: IO () }

instance IsString Command where
  fromString = Command . putStrLn
