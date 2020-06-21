{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-}
module Command where

import Control.Monad.Free
import Data.String



data CommandF a
  = Display String a
  deriving Functor

type Command = Free CommandF ()

display
  :: String -> Command
display s
  = liftF $ Display s ()

instance IsString Command where
  fromString = display
