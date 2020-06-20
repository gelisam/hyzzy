{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main where

import Control.Monad.Catch
import Data.Foldable
import Data.Function
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

processInput
  :: String
  -> InterpreterT IO ()
processInput ":help" = do
  liftIO $ putStrLn ":browse   List the commands available in the current room."
  liftIO $ putStrLn ":help     List the meta-commands."
processInput ":browse" = do
  moduleElems <- getModuleExports "Commands"
  for_ moduleElems $ \case
    Fun functionName -> do
      typeName <- typeOf functionName
      liftIO $ putStrLn $ functionName ++ " :: " ++ typeName
    _ -> do
      pure ()
processInput input = do
  r <- try $ interpret input (as :: Command)
  case r of
    Left (UnknownError e) -> do
      liftIO $ putStrLn e
    Left (WontCompile es) -> do
      -- does it at least type-check?
      r <- try $ typeOf input
      case (r :: Either InterpreterError String) of
        Left _ -> do
          -- show interpret's error, not typeOf's
          for_ es $ \e -> do
            liftIO $ putStrLn $ errMsg e
        Right typeName -> do
          -- e.g. "open :: Door -> Command"
          liftIO $ putStrLn $ input ++ " :: " ++ typeName
    Left (NotAllowed e) -> do
      liftIO $ putStrLn e
    Left (GhcException e) -> do
      liftIO $ putStrLn e
    Right command -> do
      liftIO $ runCommand command

main
  :: IO ()
main = do
  putStrLn "A toy text adventure where commands have Haskell types."
  putStrLn "Type \":help\" to view the meta-commands."
  r <- runInterpreter $ do
    setImportsQ [ ("Prelude", Nothing)
                , ("Commands", Nothing)
                ]
    runInputT haskelineSettings $ fix $ \loop -> do
      getInputLine "> " >>= \case
        Nothing -> pure ()
        Just input -> do
          lift $ processInput input
          loop
  case r of
    Left e -> do
      print e
      exitFailure
    Right () -> do
      pure ()
