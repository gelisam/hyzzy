module Main where

import Control.Monad.Catch
import Language.Haskell.Interpreter
import System.Console.Haskeline
import System.Exit


haskelineSettings
  :: Settings (InterpreterT IO)
haskelineSettings
  = setComplete completionFunc defaultSettings

completionFunc
  :: (String, String)
  -> InterpreterT IO (String, [Completion])
completionFunc (lhs, _) = do
  pure (lhs, [])

main
  :: IO ()
main = do
    r <- runInterpreter $ do
      setImportsQ [ ("Prelude", Nothing)
                  , ("Rooms.Garden", Nothing)
                  ]
      runInputT haskelineSettings loop
    case r of
      Left e -> do
        print e
        exitFailure
      Right () -> do
        pure ()
  where
    loop :: InputT (InterpreterT IO) ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> do
          r <- try $ lift $ interpret input (as :: IO ())
          case r of
            Left e -> do
              liftIO $ print (e :: InterpreterError)
            Right action -> do
              liftIO action
          loop
