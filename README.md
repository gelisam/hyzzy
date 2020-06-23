hyzzy
===

A framework for defining text adventures via Haskell files. Play by combining functions, not by guessing phrases.
 
Two small example games are included, `games/castle` and `games/maze`. An example game plays like this:

    $ stack run hyzzy games/castle
    A toy text adventure where commands have Haskell types.
    Type ":help" to view the meta-commands.
    > look
    You stand in front of the castle's heavy door.
    > open door
    It's locked.
    > unlock
    unlock :: Key -> Door -> Command
    > :inventory
    key :: Key
    > unlock key door
    That random key you happened to carry in your pockets happens to fit the lock. What are the odds?
    > open door
    The door opens with a drawn-out yawn.
    > enter door
    You're in!

If you want to make your own games, here's how the above game is defined:

```haskell
intro :: Command
intro = Command $ do
  display "A toy text adventure where commands have Haskell types."
  display "Type \":help\" to view the meta-commands."

  addToInventory "key" Key ()

look :: Command
look = "You stand in front of the castle's heavy door."


newtype Door = Door (Object DoorFields)
data DoorFields = DoorFields
  { doorLocked :: Bool
  , doorOpened :: Bool
  }
  deriving Generic

door :: Instance Door
door = Instance Door $ DoorFields
  { doorLocked = True
  , doorOpened = False
  }

enter :: Door -> Command
enter door = Command $ do
  DoorFields {..} <- getFields door
  if doorOpened
    then do
      display "You're in!"
      goToRoom "Castle"
    else do
      display "The door is closed."

open :: Door -> Command
open door = Command $ do
  DoorFields {..} <- getFields door
  if | doorLocked -> display "It's locked."
     | doorOpened -> display "It's already opened."
     | otherwise  -> do
       setField Door door #doorOpened True
       display "The door opens with a drawn-out yawn."


newtype Key = Key (Object ())

unlock :: Key -> Door -> Command
unlock key door = Command $ do
  consume Key key
  setField Door door #doorLocked False
  display "That random key you happened to carry in your pockets happens to fit the lock. What are the odds?"
```
