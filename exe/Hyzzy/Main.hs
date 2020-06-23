{-# LANGUAGE DeriveGeneric, GADTs, GeneralizedNewtypeDeriving, LambdaCase, OverloadedLabels, RankNTypes, RecordWildCards, TypeApplications, ViewPatterns #-}
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
import Data.Proxy
import Data.Unique
import GHC.Generics (Generic)
import Language.Haskell.Interpreter (Interpreter, InterpreterError, InterpreterT, ModuleName)
import System.Console.Haskeline
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import Text.Printf
import Type.Reflection (SomeTypeRep, TyCon)
import qualified Data.Map as Map
import qualified Language.Haskell.Interpreter as I
import qualified Type.Reflection as Typeable

import Hyzzy.Command
import Hyzzy.Object
import Hyzzy.Room


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
  liftI $ I.interpret code I.infer

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
  = Typeable.withTypeable typeRep $ extendCtx termName a


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

newObject
  :: Ctor object fields
  -> fields
  -> IO (Unique, object)
newObject ctor fields = do
  unique <- newUnique
  object <- Object unique <$> newIORef fields
  pure (unique, ctor object)


data World = World
  { playerInventory :: Inventory
  , playerLocation  :: RoomName
  , worldRooms      :: Map RoomName Room
  }
  deriving Generic

currentRoom
  :: Lens' World Room
currentRoom = lens getter setter
  where
    getter
      :: World -> Room
    getter world
      = (world ^. #worldRooms) ! (world ^. #playerLocation)

    setter
      :: World -> Room -> World
    setter world room
      = over #worldRooms
             (Map.insert (world ^. #playerLocation) room)
             world

currentCtx
  :: M Ctx
currentCtx = do
  inventory <- liftW $ use #playerInventory
  room <- liftW $ use currentRoom
  pure $ extendCtxWithInventory inventory
       $ extendCtxWithRoom room
       $ emptyCtx


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
  :: World -> M a -> Interpreter a
runM world
  = flip evalStateT world
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
    (tell =<<) $ lift $ liftW $ Map.keys <$> use (currentRoom . #roomCommands)
    (tell =<<) $ lift $ liftW $ Map.keys <$> use (currentRoom . #roomObjectNames)
    (tell =<<) $ lift $ liftW $ Map.keys <$> use (#playerInventory . #inventoryNames)
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
availableCommandNames = liftI $ do
  loadModuleDefs "Commands"

runCommand
  :: Command -> M ()
runCommand
  = foldFree (lowerM . hoistCoyoneda runCommandF)
  . unCommand

runCommandF
  :: CommandF a -> M a
runCommandF = \case
  Display s -> do
    liftIO $ putStrLn s

  AddToInventory name ctor fields -> do
    (unique, object) <- liftIO $ newObject ctor fields

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

    liftW $ modifying (currentRoom . #roomObjectNames)
          $ Map.filter (/= objectId)
    liftW $ modifying (currentRoom . #roomObjectInstances)
          $ Map.delete objectId

  GoToRoom roomName -> do
    liftW $ #playerLocation .= roomName


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
    , MetaCommand ":browse" "List the commands which are available in every room." $ do
        commandNames <- availableCommandNames
        for_ commandNames $ \commandName -> do
          typeName <- liftI $ typeNameOf commandName
          liftIO $ putStrLn $ commandName ++ " :: " ++ typeName
    , MetaCommand ":look" "List the room-specific commands and objects." $ do
        room <- liftW $ use currentRoom
        let dynamicList = roomToCommandList room ++ roomToObjectList room
        for_ dynamicList $ \(commandName, dynamic) -> do
          let typeName = show $ dynTypeRep dynamic
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


typeNameOf
  :: TermName
  -> Interpreter TypeName
typeNameOf
  = I.typeOf

someTypeRepOf
  :: TermName
  -> Interpreter SomeTypeRep
someTypeRepOf termName = do
  dynTypeRep <$> loadDynamic termName

tyConOf
  :: TermName
  -> Interpreter TyCon
tyConOf termName = do
  Typeable.someTypeRepTyCon <$> someTypeRepOf termName

loadDynamic
  :: TermName
  -> Interpreter Dynamic
loadDynamic termName = do
  I.interpret ("toDyn " ++ termName) I.infer

instanceTyCon
  :: TyCon
instanceTyCon
  = Typeable.someTypeRepTyCon
  $ Typeable.someTypeRep
  $ Proxy @(Instance ())

loadSomeInstance
  :: TermName
  -> Interpreter (Maybe SomeInstance)
loadSomeInstance termName = do
  tyCon <- tyConOf termName
  if tyCon == instanceTyCon
    then Just <$> do
      I.interpret ("SomeInstance " ++ termName) I.infer
    else do
      pure Nothing

loadModuleDefs
  :: ModuleName
  -> Interpreter [TermName]
loadModuleDefs moduleName = do
  moduleElems <- I.getModuleExports moduleName
  execWriterT $ do
    for_ moduleElems $ \case
      I.Fun termName -> do
        tell [termName]
      _ -> do
        pure ()

loadModule
  :: ModuleName -> Interpreter (Map TermName Dynamic)
loadModule moduleName = do
  I.setImports ["Data.Dynamic", moduleName]
  termNames <- loadModuleDefs moduleName
  execWriterT $ do
    for_ termNames $ \termName -> do
      dynamic <- lift $ loadDynamic termName
      tell $ Map.singleton termName dynamic


data Room = Room
  { roomCommands        :: Map String Dynamic
  , roomObjectNames     :: Map String Unique
  , roomObjectInstances :: Map Unique Dynamic
  }
  deriving Generic

instance Semigroup Room where
  Room x1 x2 x3 <> Room y1 y2 y3
    = Room (x1 <> y1)
           (x2 <> y2)
           (x3 <> y3)

instance Monoid Room where
  mempty = emptyRoom

emptyRoom
  :: Room
emptyRoom
  = Room mempty
         mempty
         mempty

roomToCommandList
  :: Room -> [(TermName, Dynamic)]
roomToCommandList
  = Map.toList . roomCommands

roomToObjectList
  :: Room -> [(TermName, Dynamic)]
roomToObjectList (Room {..})
  = [ (name, roomObjectInstances ! unique)
    | (name, unique) <- Map.toList roomObjectNames
    ]

roomModule
  :: RoomName -> String
roomModule roomName
  = "Rooms." ++ unRoomName roomName

loadRoom
  :: RoomName -> Interpreter Room
loadRoom roomName = do
  let moduleName = roomModule roomName
  I.setImports ["Data.Dynamic", "Hyzzy.Object", moduleName]
  termNames <- loadModuleDefs moduleName
  execWriterT $ do
    for_ termNames $ \termName -> do
      maybeSomeInstance <- lift $ loadSomeInstance termName
      case maybeSomeInstance of
        Just (SomeInstance (Instance ctor fields)) -> do
          (unique, object) <- liftIO $ newObject ctor fields
          tell $ emptyRoom
            { roomObjectNames     = Map.singleton termName unique
            , roomObjectInstances = Map.singleton unique (toDyn object)
            }
        Nothing -> do
          dynamic <- lift $ loadDynamic termName
          tell $ emptyRoom
            { roomCommands = Map.singleton termName dynamic
            }

extendCtxWithRoom
  :: Room -> Ctx -> Ctx
extendCtxWithRoom room ctx
  = foldr (uncurry extendCtxWithDynamic) ctx
  $ roomToCommandList room
 ++ roomToObjectList room


initialize
  :: GamePath -> Interpreter World
initialize gamePath = do
  roomBasePaths <- liftIO $ listDirectory (gamePath </> "Rooms")
  let roomNames = RoomName
              <$> dropExtension
              <$> roomBasePaths
  let roomPaths = (gamePath </>)
              <$> ("Rooms" </>)
              <$> roomBasePaths
  I.loadModules $ [ gamePath </> "Commands.hs"
                  , gamePath </> "Objects.hs"
                  , gamePath </> "PublicObjects.hs"
                  , gamePath </> "Start.hs"
                  ]
               ++ roomPaths

  rooms <- execWriterT $ do
    for_ roomNames $ \roomName -> do
      room <- lift $ loadRoom roomName
      tell $ Map.singleton roomName room

  I.setImports ["Hyzzy.BridgeTypes", "Start"]
  startingRoom <- I.interpret "startingRoom" I.infer

  pure $ World
    { playerInventory = initialInventory
    , playerLocation  = startingRoom
    , worldRooms      = rooms
    }

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
    Left (I.UnknownError e) -> do
      liftIO $ putStrLn e
    Left (I.WontCompile es) -> do
      -- does it at least type-check?
      r <- liftI $ try $ typeNameOf input
      case (r :: Either InterpreterError String) of
        Left _ -> do
          -- show interpret's error, not typeNameOf's
          for_ es $ \e -> do
            liftIO $ putStrLn $ I.errMsg e
        Right typeName -> do
          -- e.g. "open :: Door -> Command"
          liftIO $ putStrLn $ input ++ " :: " ++ typeName
    Left (I.NotAllowed e) -> do
      liftIO $ putStrLn e
    Left (I.GhcException e) -> do
      liftIO $ putStrLn e
    Right command -> do
      runCommand command

play
  :: GamePath -> IO ()
play gamePath = do
  r <- I.runInterpreter $ do
    world <- initialize gamePath
    runM world $ do
      liftI $ I.setImports ["Hyzzy.BridgeTypes", "Start"]
      intro <- liftI $ I.interpret "intro" I.infer
      runCommand intro

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
            roomName <- lift . liftW $ use #playerLocation
            lift . liftI $ I.setImports
                         $ ["Hyzzy.BridgeTypes", "Commands", "PublicObjects"]
                        ++ [roomModule roomName]
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
