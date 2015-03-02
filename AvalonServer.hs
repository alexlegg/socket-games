{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module AvalonServer
    ( avalonServer
    ) where

import Prelude
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.State.Strict (StateT, mzero)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.SocketIO as SocketIO
import Import
import Database.Persist.MongoDB

data BThing = BThing !Int

instance Aeson.ToJSON BThing where
    toJSON (BThing x) = Aeson.object [ "bThing" .= x ]

data Login = Login Text.Text

instance Aeson.FromJSON Login where
    parseJSON (Aeson.Object x)  = Login <$> x .: "name"
    parseJSON _                 = mzero

data GameList = GameList [LobbyGame]

instance Aeson.ToJSON GameList where
    toJSON (GameList games) = Aeson.object [ "games" .= games ]

data LobbyGame = LobbyGame String Text.Text Int

instance Aeson.ToJSON LobbyGame where
    toJSON (LobbyGame gid nm p)   = Aeson.object [
          "id"          .= gid
        , "name"        .= nm
        , "num_players" .= p
        ]

toLobbyGame :: Entity Game -> LobbyGame
toLobbyGame g = LobbyGame (show (keyToOid (entityKey g))) gname (length (gamePlayers v))
    where
        v       = entityVal g
        gname   = Text.intercalate ", " (map playerName (gamePlayers v))

toPlayerKey :: Text.Text -> Maybe (Key Player)
toPlayerKey (readMayObjectId -> Just oid)   = Just $ oidToKey oid
toPlayerKey _                               = Nothing

sendGameList :: ReaderT SocketIO.Socket (HandlerT App IO) ()
sendGameList = do
    liftIO $ putStrLn "send game list1"
    games <- lift $ runDB $ selectList ([] :: [Filter Game]) []
    liftIO $ putStrLn "send game list2"
    liftIO $ putStrLn (show games)
    SocketIO.emit "gamelist" (GameList (map toLobbyGame games))
    return ()

getSocketPlayer :: ReaderT SocketIO.Socket (HandlerT App IO) (Maybe (Entity Player))
getSocketPlayer = do
    socket <- ask
    lift $ runDB $ selectFirst [PlayerSocket ==. (SocketIO.socketId socket)] []

avalonServer :: StateT (SocketIO.RoutingTable (HandlerT App IO)) (ReaderT SocketIO.Socket (HandlerT App IO)) ()
avalonServer = do
    playerId <- lookupSession "player_id"

    case maybe Nothing toPlayerKey playerId of
        Nothing     -> SocketIO.emit "bad_login" (empty :: String)
        (Just pid)  -> do
            p <- lift $ lift $ runDB $ get pid
            case p of
                (Just _)   -> do
                    lift $ sendGameList
                Nothing         -> do
                    SocketIO.emit "bad_login" (empty :: String)

    SocketIO.on "blahblah" $ do
        SocketIO.broadcast "blah blah" (BThing 4444)

    SocketIO.on "login" $ \(Login name) -> do
        socket <- ask
        p <- lift $ runDB $ selectFirst [PlayerName ==. name] []
        case p of
            Nothing     -> do
                _ <- lift $ runDB $ insert (Player name (SocketIO.socketId socket) Nothing)
                return ()
            Just player -> do
                _ <- lift $ runDB $ update (entityKey player) [PlayerSocket =. SocketIO.socketId socket]
                return ()

        sendGameList

    SocketIO.on "newgame" $ do
        liftIO $ putStrLn "newgame"
        p <- getSocketPlayer
        case p of
            Nothing     -> return ()
            Just player -> do
                liftIO $ putStrLn "player"
                _ <- lift $ runDB $ insert (Game GameLobby [entityVal player])
                sendGameList

