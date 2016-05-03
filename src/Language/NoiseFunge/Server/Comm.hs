{-
    This source file is a part of the noisefunge programming environment.

    Copyright (C) 2015 Rev. Johnny Healey <rev.null@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE FlexibleContexts #-}

module Language.NoiseFunge.Server.Comm (Subscription(..),
                                        Request(..),
                                        Response(..),
                                        withAPIConnection,
                                        Conn,
                                        addProgram,
                                        sendBinary',
                                        requestReset,
                                        streamEvents) where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as ST
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Binary
import Data.ByteString.Lazy as BS

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString

import System.Environment
import System.Timeout

import Language.NoiseFunge.Beat
import Language.NoiseFunge.Befunge
import Language.NoiseFunge.Befunge.Process


data Subscription = Stats | Deltas
    deriving (Read, Show, Eq, Ord)

instance Binary Subscription where
    get = getSub <$> getWord8 where
        getSub 0 = Stats
        getSub 1 = Deltas
        getSub _ = error "Bad Subscription"
    put Stats = putWord8 0
    put Deltas = putWord8 1

data Request =
    Subscribe Subscription (Maybe Beat)
  | StartProgram String String String ProgArray
  | StopProgram (Maybe Word32) (Maybe String) (Maybe String)
  | SendReset
  deriving (Show, Eq, Ord)

instance Binary Request where
    get = getWord8 >>= getReq where
        getReq 0 = Subscribe <$> get <*> get
        getReq 1 = StartProgram <$> get <*> get <*> get <*> get
        getReq 2 = StopProgram <$> get <*> get <*> get
        getReq 3 = return SendReset
        getReq _ = error "Bad request"
    put (Subscribe s b) =
        putWord8 0 >> put s >> put b
    put (StartProgram n ib ob a) =
        putWord8 1 >> put n >> put ib >> put ob >> put a
    put (StopProgram p n r) =
        putWord8 2 >> put p >> put n >> put r
    put SendReset =
        putWord8 3

data Response =
    Catchup !Beat !PID ProgArray Delta
  | Change !Beat !PID Delta
  | Dead !Beat !PID (Maybe String)
  | NewProcess !PID
  | NextBeat !Beat
  | ProcessStats !Beat !BefungeStats
  | TickStats !Beat !Word32 !Word32 !Word32 !Word32
  | Reset
  deriving (Show, Eq, Ord)

instance Binary Response where
    get = getWord8 >>= getResp where
        getResp 0 = Catchup <$> get <*> get <*> get <*> get
        getResp 1 = Change <$> get <*> get <*> get
        getResp 2 = Dead <$> get <*> get <*> get
        getResp 3 = NewProcess <$> get
        getResp 4 = NextBeat <$> get
        getResp 5 = ProcessStats <$> get <*> get
        getResp 6 = TickStats <$> get <*> get <*> get <*> get <*> get
        getResp 7 = return Reset
        getResp _ = error "Bad Response"
    put (Catchup a b c d) = putWord8 0 >> put a >> put b >> put c >> put d
    put (Change a b c) = putWord8 1 >> put a >> put b >> put c
    put (Dead a b c) = putWord8 2 >> put a >> put b >> put c
    put (NewProcess a) = putWord8 3 >> put a
    put (NextBeat a) = putWord8 4 >> put a
    put (ProcessStats a b) = putWord8 5 >> put a >> put b
    put (TickStats a b c d e) =
        putWord8 6 >> put a >> put b >> put c >> put d >> put e
    put Reset = putWord8 7

data Conn = Conn Socket SockAddr

withAPIConnection :: (Conn -> IO a) -> IO ()
withAPIConnection fn = do
    host <- getEnv "NOISEFUNGE_HOST"
    servHost <- getEnv "NOISEFUNGE_SERVER_HOST"
    servPort <- getEnv "NOISEFUNGE_SERVER_PORT"
    ais <- getAddrInfo Nothing (Just host) Nothing
    sais <- getAddrInfo Nothing (Just servHost) (Just servPort)
    case (ais, sais) of
        ([],__) -> error "Invalid NOISEFUNGE_HOST"
        (_, []) -> error
            "Invalid NOISEFUNGE_SERVER_HOST or NOISEFUNGE_SERVER_PORT"
        ((ai:_), (sai:_)) -> do
            s <- socket (addrFamily ai) Datagram defaultProtocol
            bindSocket s (addrAddress ai)
            void $ fn (Conn s (addrAddress sai))

runBuffered :: Monad m => ST.StateT [b] m a -> m a
runBuffered bst = ST.evalStateT bst []

sendBinary :: (MonadIO m, Functor m) => Binary b =>
    b -> Conn -> ST.StateT [b1] m ()
sendBinary b (Conn s dest) = do
    let b' = toStrict $ encode b
    void $ liftIO $ sendTo s b' dest

sendBinary' :: Binary b => b -> Conn -> IO ()
sendBinary' b (Conn s dest) = do
    let b' = toStrict $ encode b
    void $ sendTo s b' dest

waitForBinary :: (Functor m, MonadIO m) => Binary b => Conn ->
    Maybe Int -> ST.StateT [b] m (Maybe b)
waitForBinary (Conn s _) timeo = runMaybeT readBinary where
    readBinary = readFromBuf <|> readFromSock
    readFromBuf = do
        buff <- lift $ ST.get
        case buff of
            [] -> fail "empty"
            (x:xs) -> do
                ST.put xs
                return x
    readFromSock = do
        (bs, _) <- case timeo of
            Just t -> MaybeT . liftIO . timeout t $ recvFrom s 32768
            Nothing -> liftIO $ recvFrom s 32768
        lift $ decodeAll (fromChunks [bs])
        readBinary

    decodeAll bs
        | BS.null bs = return ()
        | otherwise = case (decodeOrFail bs) of
            Left (_,_,err) -> fail err
            Right (rest,_,a) -> do
                decodeAll rest
                ST.modify (a:)

addProgram :: Conn -> String -> String -> String -> [String] -> IO PID
addProgram conn name inbuf outbuf arr = do
    runBuffered $ do
        sendBinary (StartProgram name inbuf outbuf (makeProgArray arr)) conn
        Just (NewProcess p) <- waitForBinary conn Nothing
        return p

requestReset :: Conn -> IO ()
requestReset conn = runBuffered $ do
    sendBinary SendReset conn

streamEvents :: (Functor m, MonadIO m) => [Subscription] -> Conn ->
    (Response -> m a) -> m ()
streamEvents subs conn fn = runBuffered $ initialize where
    initialize = do
        subscribe Nothing
        wait
    subscribe b = do
        forM_ subs $ \sub -> do
            sendBinary (Subscribe sub b) conn
    wait = waitForBinary conn (Just 1000000) >>= handle
    handle Nothing = initialize
    handle (Just r@(NextBeat b)) = do
        void . lift $ fn r
        subscribe (Just b)
        wait
    handle (Just r) = do
        void . lift $ fn r
        wait
    
