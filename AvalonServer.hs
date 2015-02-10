{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module AvalonServer
    ( avalonServer
    ) where

import Prelude
import Control.Applicative
import Control.Monad.IO.Class
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

data Login = Login {
      name    :: Text.Text
    }

instance Aeson.FromJSON Login where
    parseJSON (Aeson.Object x)  = Login <$> x .: "name"
    parseJSON _                 = mzero

toPlayerKey :: Text.Text -> Maybe (Key Player)
toPlayerKey (readMayObjectId -> Just oid)   = Just $ oidToKey oid
toPlayerKey _                               = Nothing

sendGameList = do
    games <- lift $ lift $ runDB $ selectList ([] :: [Filter Game]) []
    return ()

avalonServer :: StateT (SocketIO.RoutingTable (HandlerT App IO)) (ReaderT SocketIO.Socket (HandlerT App IO)) ()
avalonServer = do
    playerId <- lookupSession "player_id"

    case playerId of
        Nothing     -> SocketIO.emit "bad_login" (empty :: String)
        (Just pid)  -> do
            let (Just id) = toPlayerKey pid
            p <- lift $ lift $ runDB $ get id
            case p of
                (Just player)   -> do
                    sendGameList
                Nothing         -> do
                    SocketIO.emit "bad_login" (empty :: String)

---    game <- lift $ lift $ runDB $ insert $ Game 22
---    player <- lift $ lift $ runDB $ insert $ Player "AAAAA" "sdfsdf" game

    SocketIO.on "blahblah" $ do
        SocketIO.broadcast "blah blah" (BThing 4444)

    SocketIO.on "login" $ do
        socket <- ask
        liftIO $ putStrLn (show (SocketIO.socketId socket))
        liftIO $ putStrLn "login recvd"
        SocketIO.emit "test response" (BThing 33)
