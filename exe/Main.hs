{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, LambdaCase, OverloadedLabels, RecordWildCards, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main where

import Control.Exception (AsyncException(UserInterrupt))
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.Dynamic
import Data.Foldable
import Data.Function
import Data.Generics.Labels ()
import Data.List
import Data.Map (Map)
import Data.Maybe
import GHC.Generics (Generic)
import Language.Haskell.Interpreter
  ( GhcError(errMsg), InterpreterError(..), ModuleElem(Fun), Interpreter, InterpreterT, as
  , getModuleExports, interpret, runInterpreter, setImportsQ, typeOf
  )
import System.Console.Haskeline
import System.Exit
import qualified Data.Map as Map

import Command
import Objects


type TypeName = String


type Inventory = Map String Dynamic

initialInventory
  :: Inventory
initialInventory
  = Map.fromList
    [ ("door", toDyn Door)  -- TODO: put the door in the environment, not in the inventory!
    , ("key", toDyn Key)
    ]


data World = World
  { playerInventory :: Inventory
  }
  deriving Generic

initialWorld
  :: World
initialWorld
  = World
    { playerInventory = initialInventory
    }


newtype M a = M
  { unM :: StateT World (InterpreterT IO) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadIO
           , MonadThrow, MonadCatch, MonadMask
           )

liftW
  :: State World a
  -> M a
liftW
  = M . hoist (pure . runIdentity)

liftI
  :: Interpreter a
  -> M a
liftI
  = M . lift

runM
  :: M a
  -> IO (Either InterpreterError a)
runM
  = runInterpreter
  . flip evalStateT initialWorld
  . unM


haskelineSettings
  :: Settings M
haskelineSettings
  = setComplete completionFunc defaultSettings

isWordChar
  :: Char -> Bool
isWordChar c = isAlphaNum c || c == ':'

completionFunc
  :: (String, String)
  -> M (String, [Completion])
completionFunc (reversedLhs, _) = do
  let reversedWordPrefix = takeWhile isWordChar reversedLhs
  let wordPrefix = reverse reversedWordPrefix
  names <- execWriterT $ do
    tell $ toListOf (each . #metaCommandName) metaCommands
    tell =<< lift availableCommandNames
    (tell =<<) $ lift $ liftW $ use (#playerInventory . to Map.keys)
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
  :: M [String]
availableCommandNames = execWriterT $ do
  moduleElems <- lift . liftI $ getModuleExports "Commands"
  for_ moduleElems $ \case
    Fun functionName -> do
      tell [functionName]
    _ -> do
      pure ()


data MetaCommand = MetaCommand
  { metaCommandName   :: String
  , metaCommandHelp   :: String
  , metaCommandAction :: M ()
  }
  deriving Generic

metaCommands
  :: [MetaCommand]
metaCommands
  = [ MetaCommand ":help" "List the meta-commands." $ do
        let column1Width = fromMaybe 0
                         $ maximumOf (each . #metaCommandName . to length) metaCommands
        for_ metaCommands $ \(MetaCommand {..}) -> do
          liftIO $ putStrLn $ take (column1Width + 2) (metaCommandName ++ repeat ' ')
                           ++ metaCommandHelp
    , MetaCommand ":browse" "List all the commands you can perform." $ do
        commandNames <- availableCommandNames
        for_ commandNames $ \commandName -> do
          typeName <- liftI $ typeOf commandName
          liftIO $ putStrLn $ commandName ++ " :: " ++ typeName
    , MetaCommand ":inventory" "List the objects you have picked up so far." $ do
        inventory <- liftW $ use #playerInventory
        for_ (Map.toList inventory) $ \(objectName, object) -> do
          let typeName = show . dynTypeRep $ object
          liftIO $ putStrLn $ objectName ++ " :: " ++ typeName
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
  -> M ()
processInput "" = do
  pure ()
processInput (lookupMetaCommand -> Just metaCommand) = do
  metaCommandAction metaCommand
processInput input = do
  r <- liftI $ try $ interpret input (as :: Command)
  case r of
    Left (UnknownError e) -> do
      liftIO $ putStrLn e
    Left (WontCompile es) -> do
      -- does it at least type-check?
      r <- liftI $ try $ typeOf input
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
  r <- runM $ do
    liftI $ setImportsQ [ ("Prelude", Nothing)
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
