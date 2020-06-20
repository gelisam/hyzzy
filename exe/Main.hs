{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main where

import Control.Exception (AsyncException(UserInterrupt))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Writer
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
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
completionFunc (reversedLhs, _) = do
  let reversedWordPrefix = takeWhile isAlphaNum reversedLhs
  let wordPrefix = reverse reversedWordPrefix
  commandNames <- availableCommandNames
  completions <- execWriterT $ do
    for_ commandNames $ \commandName -> do
      when (wordPrefix `isPrefixOf` commandName) $ do
        let completion = Completion
              { replacement = drop (length wordPrefix) commandName
              , display     = commandName
              , isFinished  = True
              }
        tell [completion]
  pure (reversedLhs, completions)

availableCommandNames
  :: InterpreterT IO [String]
availableCommandNames = execWriterT $ do
  moduleElems <- lift $ getModuleExports "Commands"
  for_ moduleElems $ \case
    Fun functionName -> do
      tell [functionName]
    _ -> do
      pure ()

processInput
  :: String
  -> InterpreterT IO ()
processInput "" = do
  pure ()
processInput ":help" = do
  liftIO $ putStrLn ":browse   List the commands available in the current room."
  liftIO $ putStrLn ":help     List the meta-commands."
  liftIO $ putStrLn ":quit     Abandon the quest (Ctrl-D works too)."
processInput ":browse" = do
  commandNames <- availableCommandNames
  for_ commandNames $ \commandName -> do
    typeName <- typeOf commandName
    liftIO $ putStrLn $ commandName ++ " :: " ++ typeName
processInput ":quit" = do
  liftIO exitSuccess
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
      r <- try $ getInputLine "> "
      case r of
        Left UserInterrupt -> do
          -- clear the line on Ctrl-C
          loop
        Left e -> do
          throwM e
        Right Nothing -> do
          -- eof
          pure ()
        Right (Just input) -> do
          lift $ processInput
               $ dropWhile (== ' ')
               $ dropWhileEnd (== ' ')
               $ input
          loop
  case r of
    Left e -> do
      print e
      exitFailure
    Right () -> do
      pure ()
