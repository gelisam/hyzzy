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

If you want to make your own games, here's how the above room is defined:

```haskell
look :: Command
look = "You stand in front of the castle's heavy door."

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
```
