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

module Language.NoiseFunge.Engine (initNF, NoiseFungeEngine,
                                   beatEvents, startProgram,
                                   stopProgram,
                                   beatVar) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Trans
import Control.Monad.Writer

import Data.Maybe
import Data.Word

import Language.NoiseFunge.ALSA
import Language.NoiseFunge.Beat
import Language.NoiseFunge.Befunge
import Language.NoiseFunge.Befunge.Process


data NoiseFungeEngine = NFE {
    _alsaThread :: ALSAThread,
    _funger     :: BefungeThread
    } 

$(makeLenses ''NoiseFungeEngine)

beatVar :: Getter NoiseFungeEngine (TVar Beat) 
beatVar = alsaThread.clock

-- Initiate the NoiseFungeEngine
initNF :: ALSAThreadConfig -> OperatorParams ->
    IO NoiseFungeEngine
initNF conf pars = do
    -- start the ALSA thread
    alsa <- startALSAThread conf
    -- start the befunge thread
    bft <- startBefungeThread (conf^.alsaTempo) pars
    let nfe = NFE alsa bft

    -- read from the alsa clock and write the next beat to the
    -- noisefunge thread so that it will start computing the next
    -- set of events.
    void . forkIO $ beatStateHandler (alsa^.clock) $ \nextBeat -> do
        liftIO . atomically $ do
            b <- nextBeat
            putTMVar (bft^.beatIn) b
            return b
    delts <- atomically $ dupTChan (bft^.deltaOut)

    -- fork a thread to read the deltas from the noisefunge thread and
    -- write them to the ALSA output thread.
    void . forkIO $ forever . atomically $ do
        (b, prs, _, _) <- readTChan delts
        forM_ prs $ \(_, _, delt) -> do
            let evs = delt^.events
            forM_ evs $ \ev -> do
                case ev of 
                    NoteEvent n -> writeTChan (alsa^.outEvents) (b, n)
                    _ -> return ()
    return nfe

type BeatState = S.StateT Beat

beatStateHandler :: (Functor m, MonadIO m) => TVar Beat ->
    (STM Beat -> BeatState m Beat) -> m ()
beatStateHandler bvar f = do
    start <- liftIO . atomically $ readTVar bvar
    flip S.evalStateT start . void . forever $ do
        prev <- S.get
        let handler = do
                bv <- readTVar bvar
                if bv == prev then retry else return bv
        f handler >>= S.put

-- beatEvents takes a NoiseFungeEngine and a handler function and calls
-- the handler when the clock triggers a beat event.
beatEvents :: (Functor m, MonadIO m) => NoiseFungeEngine ->
    (Beat -> [(PID, ProcessState, Delta)] ->
        [(PID, Maybe String)] -> [String] -> [BefungeStats] -> m a) -> m ()
beatEvents nfe fn = bev where
    bv = nfe^.beatVar
    dout = nfe^.funger.deltaOut
    eout = nfe^.funger.errOut
    bev = beatStateHandler bv $ \nextBeat -> do
        (curr, delts, deads, errs, stats) <- liftIO . atomically $ do
            bt <- nextBeat
            (delts, deads, stats) <- getDeltas bt
            errs <- getErrs
            return (bt, delts, deads, errs, stats)
        void . lift $ fn curr delts deads errs stats
        return curr
    getDeltas bt = do
        (bt', _, _, _) <- peekTChan dout
        if bt' > bt
            then return ([], [], [])
            else do
                (_, delts, dead, stats) <- readTChan dout
                ((delts, dead, stats) <>) <$> getDeltas bt
    getErrs = do
        emp <- isEmptyTChan eout
        if emp
            then return []
            else (:) <$> readTChan eout <*> getErrs

-- StartProgram is used to send a program to the NoiseFungeEngine
startProgram :: NoiseFungeEngine -> ProgArray -> String -> String ->
    String -> IO PID
startProgram nfe arr name inbuf outbuf = do
    mv <- newEmptyMVar
    atomically $ writeTChan (nfe^.funger.commIn) $
        AddProcess arr name inbuf outbuf (Just mv)
    takeMVar mv

-- StopProgram kills noisefunge programs based on PID or name.
stopProgram :: NoiseFungeEngine -> Maybe Word32 -> Maybe String ->
    Maybe String -> IO ()
stopProgram nfe pf nf r = do
    atomically $ writeTChan (nfe^.funger.commIn) $
        KillProcess filt r
  where filt (pid, nam) = fromJust (checkPid pid <|> checkNam nam <|> Just True)
        checkPid pid = (pid ==) <$> pf
        checkNam nam = (nam ==) <$> nf

