{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs, TypeSynonymInstances #-}
module Command where

import Control.Monad.Free
import Data.Functor.Coyoneda
import Data.String



data CommandF r where
  Display :: String -> CommandF ()

type Command = Free (Coyoneda CommandF) ()

display
  :: String -> Command
display s
  = liftF $ liftCoyoneda $ Display s

instance IsString Command where
  fromString = display
