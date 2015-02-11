module PersistTypes where

import Prelude
import Database.Persist.TH

data GameState  = GameLobby 
                | GamePreGame 
                | GamePropose 
                | GameVote 
                | GameQuest 
                | GameAssassin
                | GameFinished
                deriving (Show, Eq, Read)

derivePersistField "GameState"

