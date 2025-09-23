module Lib1
    ( Command(..)
    , Quest(..)
    , examples
    ) where

-- Domain: Game
-- DSL commands for creating players, adding scores, starting levels, and defining quests.

-- Main DSL commands
data Command
  = CreatePlayer String       -- create player <name>
  | AddScore Int              -- add score <number>
  | StartLevel String         -- start level <name>
  | DefineQuest Quest         -- define quest <quest>
  | DumpExamples              -- dump examples
  deriving (Eq, Show)

-- Recursive structure for quests
data Quest
  = Task String               -- simple quest: task <description>
  | Sequence Quest Quest      -- recursive quest: sequence of two quests
  deriving (Eq, Show)

-- Example DSL programs
examples :: [Command]
examples =
  [ CreatePlayer "Mihai"
  , AddScore 100
  , StartLevel "Dungeon"
  , DefineQuest (Task "Find the key")
  , DefineQuest (Sequence (Task "Defeat the boss") (Task "Collect the reward"))
  ]
