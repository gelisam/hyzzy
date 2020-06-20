{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedLabels, RecordWildCards, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main where

import Control.Exception (AsyncException(UserInterrupt))
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Writer
import Data.Char
import Data.Foldable
import Data.Function
import Data.Generics.Labels ()
import Data.List
import Data.Maybe
import GHC.Generics (Generic)
import Language.Haskell.Interpreter
import System.Console.Haskeline
import System.Exit

import Command


haskelineSettings
  :: Settings (InterpreterT IO)
haskelineSettings
  = setComplete completionFunc defaultSettings

isWordChar
  :: Char -> Bool
isWordChar c = isAlphaNum c || c == ':'

completionFunc
  :: (String, String)
  -> InterpreterT IO (String, [Completion])
completionFunc (reversedLhs, _) = do
  let reversedWordPrefix = takeWhile isWordChar reversedLhs
  let wordPrefix = reverse reversedWordPrefix
  names <- execWriterT $ do
    tell $ toListOf (each . #metaCommandName) metaCommands
    tell =<< lift availableCommandNames
  completions <- execWriterT $ do
    for_ names $ \name -> do
      when (wordPrefix `isPrefixOf` name) $ do
        let completion = Completion
              { replacement = drop (length wordPrefix) name
              , display     = name
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


data MetaCommand = MetaCommand
  { metaCommandName   :: String
  , metaCommandHelp   :: String
  , metaCommandAction :: InterpreterT IO ()
  }
  deriving Generic

metaCommands
  :: [MetaCommand]
metaCommands
  = [ MetaCommand ":browse" "List the commands available in the current room." $ do
        commandNames <- availableCommandNames
        for_ commandNames $ \commandName -> do
          typeName <- typeOf commandName
          liftIO $ putStrLn $ commandName ++ " :: " ++ typeName
    , MetaCommand ":help" "List the meta-commands." $ do
        let column1Width = fromMaybe 0
                         $ maximumOf (each . #metaCommandName . to length) metaCommands
        for_ metaCommands $ \(MetaCommand {..}) -> do
          liftIO $ putStrLn $ take (column1Width + 2) (metaCommandName ++ repeat ' ')
                           ++ metaCommandHelp
    , MetaCommand ":quit" "Abandon the quest (Ctrl-D works too)." $ do
        liftIO exitSuccess
    ]

lookupMetaCommand
  :: String
  -> Maybe MetaCommand
lookupMetaCommand name
  = elemIndexOf (each .> selfIndex <. #metaCommandName) name metaCommands


processInput
  :: String
  -> InterpreterT IO ()
processInput "" = do
  pure ()
processInput (lookupMetaCommand -> Just metaCommand) = do
  metaCommandAction metaCommand
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
