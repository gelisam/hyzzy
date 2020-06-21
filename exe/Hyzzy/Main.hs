{-# LANGUAGE DeriveGeneric, GADTs, GeneralizedNewtypeDeriving, LambdaCase, OverloadedLabels, RankNTypes, RecordWildCards, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Hyzzy.Main where

import Control.Exception (AsyncException(UserInterrupt))
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.Dynamic
import Data.Foldable
import Data.Function
import Data.Functor.Coyoneda
import Data.Generics.Labels ()
import Data.IORef
import Data.List
import Data.Map (Map, (!))
import Data.Maybe
import Data.Unique
import GHC.Generics (Generic)
import Language.Haskell.Interpreter hiding (eval)
import System.Console.Haskeline
import System.FilePath
import System.Environment
import System.Exit
import Text.Printf
import Type.Reflection (withTypeable)
import qualified Data.Map as Map

import Hyzzy.Command
import Hyzzy.Object


type GamePath = FilePath
type TermName = String
type TypeName = String


type Code = String

newtype Ctx = Ctx
  { eval :: forall r. Typeable r
         => Code -> M r
  }

emptyCtx
  :: Ctx
emptyCtx = Ctx $ \code -> do
  liftI $ interpret code infer

extendCtx
  :: Typeable a
  => TermName
  -> a
  -> Ctx -> Ctx
extendCtx termName a ctx = Ctx $ \code -> do
  a2r <- eval ctx $ printf "\\%s -> %s" termName code
  pure $ a2r a

extendCtxWithDynamic
  :: TermName
  -> Dynamic
  -> Ctx -> Ctx
extendCtxWithDynamic termName (Dynamic typeRep a)
  = withTypeable typeRep $ extendCtx termName a


data Inventory = Inventory
  { inventoryNames :: Map TermName Unique
  , inventoryItems :: Map Unique Dynamic
  }
  deriving Generic

initialInventory
  :: Inventory
initialInventory
  = Inventory mempty mempty

inventoryToList
  :: Inventory
  -> [(TermName, Dynamic)]
inventoryToList (Inventory {..})
  = [ (name, inventoryItems ! unique)
    | (name, unique) <- Map.toList inventoryNames
    ]

extendCtxWithInventory
  :: Inventory -> Ctx -> Ctx
extendCtxWithInventory inventory ctx
  = foldr (uncurry extendCtxWithDynamic) ctx
  . inventoryToList
  $ inventory


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

currentCtx
  :: M Ctx
currentCtx = do
  inventory <- liftW $ use #playerInventory
  pure $ extendCtxWithInventory inventory emptyCtx


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
  :: M a -> IO (Either InterpreterError a)
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
    (tell =<<) $ lift $ liftW
               $ use (#playerInventory . #inventoryNames . to Map.keys)
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
  :: M [TermName]
availableCommandNames = execWriterT $ do
  moduleElems <- lift . liftI $ getModuleExports "Commands"
  for_ moduleElems $ \case
    Fun functionName -> do
      tell [functionName]
    _ -> do
      pure ()

runCommand
  :: Command -> M ()
runCommand
  = foldFree (lowerM . hoistCoyoneda runCommandF)

runCommandF
  :: CommandF a -> M a
runCommandF = \case
  Display s -> do
    liftIO $ putStrLn s
  AddToInventory name ctor fields -> do
    unique <- liftIO newUnique
    object <- liftIO
            $ ctor
          <$> Object unique
          <$> newIORef fields
    liftW $ modifying (#playerInventory . #inventoryNames)
          $ Map.insert name unique
    liftW $ modifying (#playerInventory . #inventoryItems)
          $ Map.insert unique (toDyn object)
  GetFields (Object {..}) -> do
    liftIO $ readIORef objectFields
  SetField (Object {..}) field value -> do
    liftIO $ modifyIORef objectFields (field .~ value)
  Consume (Object {..}) -> do
    liftW $ modifying (#playerInventory . #inventoryNames)
          $ Map.filter (/= objectId)
    liftW $ modifying (#playerInventory . #inventoryItems)
          $ Map.delete objectId


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
        for_ (inventoryToList inventory) $ \(objectName, object) -> do
          let typeName = show . dynTypeRep $ object
          liftIO $ putStrLn $ objectName ++ " :: " ++ typeName
    , MetaCommand ":quit" "Abandon the quest (Ctrl-D works too)." $ do
        liftIO exitSuccess
    ]

lookupMetaCommand
  :: String -> Maybe MetaCommand
lookupMetaCommand name
  = elemIndexOf (each .> selfIndex <. #metaCommandName) name metaCommands

runMetaCommand
  :: MetaCommand -> M ()
runMetaCommand
  = metaCommandAction


initialize
  :: GamePath -> M ()
initialize gamePath = do
  liftI $ do
    loadModules [ gamePath </> "Commands.hs"
                , gamePath </> "Objects.hs"
                , gamePath </> "PublicObjects.hs"
                , gamePath </> "Start.hs"
                ]

  liftI $ setImports ["Hyzzy.BridgeTypes", "Start"]
  intro <- liftI $ interpret "intro" infer
  runCommand intro

processInput
  :: String -> M ()
processInput "" = do
  pure ()
processInput (lookupMetaCommand -> Just metaCommand) = do
  runMetaCommand metaCommand
processInput input = do
  ctx <- currentCtx
  r <- try $ eval ctx input
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
      runCommand command

play
  :: GamePath -> IO ()
play gamePath = do
  r <- runM $ do
    initialize gamePath

    liftI $ setImports ["Hyzzy.BridgeTypes", "Commands", "PublicObjects"]
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


usage
  :: IO ()
usage = do
  putStrLn "usage:"
  putStrLn "  stack run hyzzy games/<game-name>"
  putStrLn ""
  putStrLn "Start the adventure described in the given folder."
  putStrLn "That folder must contain at least the following files:"
  putStrLn ""
  putStrLn "  games/<game-name>/Commands.hs"
  putStrLn "  games/<game-name>/Objects.hs"
  putStrLn "  games/<game-name>/PublicObjects.hs"
  putStrLn "  games/<game-name>/Start.hs"

main
  :: IO ()
main = do
  getArgs >>= \case
    [] -> do
      usage
    ["-h"] -> do
      usage
    ["--help"] -> do
      usage
    [gamePath] -> do
      play gamePath
    _ -> do
      usage
      exitFailure
