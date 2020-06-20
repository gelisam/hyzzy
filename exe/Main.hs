module Main where

import Control.Monad.Catch
import Data.Foldable
import Language.Haskell.Interpreter
import System.Console.Haskeline
import System.Exit

import Command


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
                  , ("Commands", Nothing)
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
          r <- try $ lift $ interpret input (as :: Command)
          case r of
            Left (UnknownError e) -> do
              liftIO $ putStrLn e
            Left (WontCompile es) -> do
              for_ es $ \e -> do
                liftIO $ putStrLn $ errMsg e
            Left (NotAllowed e) -> do
              liftIO $ putStrLn e
            Left (GhcException e) -> do
              liftIO $ putStrLn e
            Right command -> do
              liftIO $ runCommand command
          loop
