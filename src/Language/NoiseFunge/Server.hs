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

{-# LANGUAGE TemplateHaskell #-}

module Language.NoiseFunge.Server (runServer, ServerConfig(..)) where

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import qualified Control.Monad.State as ST
import Control.Monad.Writer

import Control.Lens

import Data.Binary
import Data.Int
import qualified Data.Map as M
import qualified Data.Set as S
import Data.ByteString.Lazy as BSL hiding (readFile)
import qualified Data.ByteString as BS

import System.IO

import Language.NoiseFunge.ALSA
import Language.NoiseFunge.Beat
import Language.NoiseFunge.Befunge
import Language.NoiseFunge.Befunge.VM
import Language.NoiseFunge.Befunge.Process
import Language.NoiseFunge.Engine
import Language.NoiseFunge.Server.Comm

data ServerConfig = ServerConfig {
        _serverHosts :: [(Family, SockAddr)],
        _serverALSAConfig :: ALSAThreadConfig,
        _serverVMOptions :: OperatorParams,
        _serverPreload :: [(FilePath, String, String)],
        _serverPacketSize :: Word16
    } deriving (Show, Eq, Ord)

$(makeLenses ''ServerConfig)

type Subscriptions = M.Map Beat (S.Set SockAddr)

data BinBuffer = BB {
    _currBuff :: (Int64, ByteString),
    _buffered :: [BS.ByteString] -> [BS.ByteString]
}

$(makeLenses ''BinBuffer)

bufferBinary :: Binary b => Word16 -> [b] -> [BS.ByteString]
bufferBinary ms xs = bufs $ ST.execState (buffer >> flush) initial where
    initial = BB blank id
    ms' = fromIntegral ms
    blank = (0, mempty)
    buffer = forM_ xs $ \x -> do
        let enc = encode x
            len = BSL.length enc
        bl <- use (currBuff._1)
        when (bl > 0 && (bl + len) > ms') $ flush
        zoom currBuff $ do
            _1 += len
            _2 %= (`mappend` enc)
    flush = do
        curr <- use currBuff
        case curr of
            (0, _) -> return ()
            (_, bs) -> do
                buffered %= (. (toStrict bs:))
                currBuff .= blank
    bufs bb = (bb^.buffered) []

requestHandler :: Socket -> NoiseFungeEngine -> TVar Bool ->
    TVar Subscriptions -> TVar Subscriptions -> IO ()
requestHandler s nfe rstv stats delts = forever $ do
    (bs, addr) <- recvFrom s 32768
    case decodeOrFail (fromChunks [bs]) of
        (Left (_,_,str)) -> hPutStrLn stderr ("Bad request: " ++ str)
        (Right (_,_, Subscribe sub b)) -> atomically $ do
            bt <- case b of
                Just b' -> return b'
                Nothing -> readTVar (nfe^.beatVar)
            let subs = case sub of
                    Stats -> stats
                    Deltas -> delts
                altfn = (Just . maybe (S.singleton addr) (S.insert addr))
            modifyTVar subs (M.alter altfn bt)
        (Right (_,_, StartProgram n ib ob a)) -> do
            pid <- startProgram nfe a n ib ob 
            hPutStrLn stderr ("Starting program: " ++ show pid)
            let res = toStrict $ encode $ NewProcess pid
            void $ sendTo s res addr
        (Right (_,_, StopProgram pf nf r)) -> do
            stopProgram nfe pf nf r
            hPutStrLn stderr ("Stopping Program(s): " ++ show (pf, nf, r))
        (Right (_,_, SendReset)) -> do
            atomically $ writeTVar rstv True

runServer :: ServerConfig -> IO ()
runServer conf = do
    hSetBuffering stderr LineBuffering
    let nextB = ((aconf^.alsaTempo) ##)
        bufferB = bufferBinary (conf^.serverPacketSize)
        aconf = conf^.serverALSAConfig
    nfe <- initNF aconf (conf^.serverVMOptions)
    rstv <- newTVarIO True
    servs <- forM (conf^.serverHosts) $ \(fam, addr) -> do
        s <- socket fam Datagram defaultProtocol
        bindSocket s addr
        stats <- newTVarIO M.empty
        delts <- newTVarIO M.empty
        void . forkIO $ requestHandler s nfe rstv stats delts
        return (s, stats, delts)
    forM_ (conf^.serverPreload) $ \(f, ib, ob) -> do
        pa <- (makeProgArray . lines) <$> (liftIO $ readFile f)
        startProgram nfe pa f ib ob
    hPutStrLn stderr ("Server is running.")
    beatEvents nfe $ \bt delts deads _ stats -> do
        rst <- atomically $ do
            v <- readTVar rstv
            when v $ writeTVar rstv False
            return v
        let nb = toStrict $ encode $ NextBeat (nextB bt)
        forM_ servs $ \(s, ssubs, dsubs) -> do
            when rst $ do
                let rstm = toStrict $ encode Reset
                addrs <- atomically $ do
                    daddrs <- M.elems <$> readTVar dsubs
                    saddrs <- M.elems <$> readTVar ssubs
                    return $ mconcat daddrs <> mconcat saddrs
                mapM_ (sendTo s rstm) (S.toList addrs)
            void $ forkIO $ do
                (outd, waiting) <- atomically $ do
                    subs <- readTVar dsubs
                    let (outd, waiting, rest) = M.splitLookup bt subs
                    writeTVar dsubs rest
                    return (mconcat (M.elems outd), waiting)
                let waiting' = maybe [] S.toList waiting
                    outd' = S.toList outd

                forM_ waiting' $ \client -> do
                    sendTo s nb client
                forM_ outd' $ \client -> do
                    sendTo s nb client

                let changes = bufferB $ do
                        (pid, _, d) <- delts
                        return $ Change bt pid d
                    catchus = bufferB $ do
                        (pid, ps, d) <- delts
                        return $ Catchup bt pid (ps^.mem) d 
                forM_ changes $ \ch -> do
                    forM_ waiting' $ \client -> do
                        sendTo s ch client
                forM_ catchus $ \ch -> do
                    forM_ outd' $ \client -> do
                        sendTo s ch client

                let deads' = bufferB $ do
                        (pid, r) <- deads
                        return $ Dead bt pid r

                forM_ deads' $ \dead -> do
                    forM_ waiting' $ \client -> do
                        sendTo s dead client
                    forM_ outd' $ \client -> do
                        sendTo s dead client

            void $ forkIO $ do
                waiting <- atomically $ do
                    subs <- readTVar ssubs
                    let (outd, waiting, rest) = M.splitLookup bt subs
                    writeTVar ssubs rest
                    let waiting' = maybe S.empty id waiting
                    return (S.toList $ mconcat (M.elems outd) <> waiting')
                
                forM_ waiting $ \client -> do
                    sendTo s nb client

                let stats' = bufferB $ do
                        stat <- stats
                        return $ ProcessStats bt stat

                forM_ stats' $ \stat -> do
                    forM_ waiting $ \client -> do
                        sendTo s stat client

                let agg = toStrict $ encode $ TickStats bt run' rbl' wbl' ded'
                    run' = getSum run
                    rbl' = getSum rbl
                    wbl' = getSum wbl
                    ded' = getSum ded
                    (run, rbl, wbl, ded) = execWriter $ do
                        mapM_ (tell . addStat . (^.vmExec)) stats
                    addStat ERunning = (Sum 1, mempty, mempty, mempty)
                    addStat (EHalted _) = (mempty, mempty, mempty, Sum 1)
                    addStat (ERBlock _) = (mempty, Sum 1, mempty, mempty)
                    addStat (EWBlock _) = (mempty, mempty, Sum 1, mempty)
                    
                forM_ waiting $ \client -> do
                    sendTo s agg client
