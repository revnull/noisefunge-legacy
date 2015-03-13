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

{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Language.NoiseFunge.Befunge (BefungeCommand(..),
                                    OperatorParams(..),
                                    BefungeThread, PID,
                                    BefungeStats,
                                    beatIn, commIn, deltaOut, errOut,
                                    tick, startBefungeThread) where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS

import Data.Default

import Language.NoiseFunge.Beat
import Language.NoiseFunge.Befunge.VM
import Language.NoiseFunge.Befunge.Process
import Language.NoiseFunge.Befunge.Operator

type BefungeStats = VMStats ProcessStats

tick :: Fungoid ()
tick = do
    q <- use quote
    fs <- use fnStack
    let opFn = case (q, fs) of
            (True, _) -> quoteOp
            (_, Just _) -> fnStackOp
            _ -> runOp
    first <- uses ticks (0 ==)
    ((), w) <- runWriterT $ do
        when first tellMem
        getOp >>= opFn >> move
    pid <- getPID
    st  <- getProcessState
    ticks += 1
    lift . tell $ ((pid, st, w):)
    yield

data BefungeCommand =
    AddProcess ProgArray String String String (Maybe (MVar PID))
  | KillProcess (PID -> Bool) (Maybe String)

data BefungeThread = BefungeThread {
    _beatIn   :: TMVar Beat,
    _commIn   :: TChan BefungeCommand,
    _deltaOut :: TChan (Beat, [(PID, ProcessState, Delta)],
                        [(PID, Maybe String)], [BefungeStats]),
    _errOut   :: TChan String
    }   

$(makeLenses ''BefungeThread)

data BefungeState = BefungeState {
    _bfsVM       :: FungeVM,
    _bfsOps      :: OpSet,
    _bfsBeats    :: [Beat],
    _bfsLast     :: Beat
    }

$(makeLenses ''BefungeState)

startBefungeThread :: Tempo -> OperatorParams -> IO (BefungeThread)
startBefungeThread temp params = do
    bfth <- BefungeThread <$> newEmptyTMVarIO
                          <*> newTChanIO
                          <*> newTChanIO
                          <*> newTChanIO
    vm <- newVM
    let bst = BefungeState vm operators (beats temp) def
    void . forkIO $ void $ runStateT (befungeRunner temp params bfth) bst
    return bfth

befungeRunner :: Tempo -> OperatorParams -> BefungeThread ->
    StateT BefungeState IO ()
befungeRunner temp params bfth = forever $ readIn >>= handle where
    bin = bfth^.beatIn
    cin = bfth^.commIn
    dout = bfth^.deltaOut
    readIn = liftIO $ atomically $ (Right <$> takeTMVar bin) `orElse`
        (Left <$> readTChan cin)
    beat5 = let f = (temp ##) in f . f . f . f . f
    handle (Right btin) = do
        nextBeats <- bfsBeats %%= (span (beat5 btin /=))
        forM_ nextBeats $ \bt -> do
            vm <- use bfsVM
            ops <- use bfsOps
            let (vm', ops', ds) = runRWS (advance bt vm) params ops
                dead = [(pid, msg) | (pid, msg, _) <- vm'^.deadProcesses]
                vmstats = fmap (^.processStats) <$> vm'^.vmStats
            liftIO . atomically $
                writeTChan dout (bt, (ds []), dead, vmstats)
            bfsVM .= vm'
            bfsOps .= ops'
        bfsLast .= btin
    handle (Left (AddProcess arr name inbuf outbuf mv)) = do
        let p = befungeProgram arr inbuf outbuf
        newp <- bfsVM %%= (addProcess name p)
        let pid = newp^.procID
        case mv of
            Nothing -> return ()
            Just mv' -> liftIO $ putMVar mv' pid
    handle (Left (KillProcess fn r)) = do
        let r' = ("Killed" ++) <$> (((": "++) <$> r) <|> Just "")
            fn' p = if fn (p^.procID) then kill r' p else p
            killall = fmap fn'
        zoom bfsVM $ do
            processQueue %= killall
            zoom (buffers.traverse) $ do
                readQueue %= killall
                writeQueue %= killall

befungeProgram :: ProgArray -> String -> String -> FungeProgram
befungeProgram arr inp out = program ps $ forever tick
  where ps = makeProcessState arr inp out

