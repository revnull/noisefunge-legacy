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

import Data.Default
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

initNF :: Tempo -> IO NoiseFungeEngine
initNF tempo = do
    alsa <- startALSAThread tempo
    bft <- startBefungeThread tempo def
    let nfe = NFE alsa bft
    void . forkIO $ beatStateHandler (alsa^.clock) $ \nextBeat -> do
        liftIO . atomically $ do
            b <- nextBeat
            putTMVar (bft^.beatIn) b
            return b
    delts <- atomically $ dupTChan (bft^.deltaOut)
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

startProgram :: NoiseFungeEngine -> ProgArray -> String -> String ->
    String -> IO PID
startProgram nfe arr name inbuf outbuf = do
    mv <- newEmptyMVar
    atomically $ writeTChan (nfe^.funger.commIn) $
        AddProcess arr name inbuf outbuf (Just mv)
    takeMVar mv

stopProgram :: NoiseFungeEngine -> Maybe Word32 -> Maybe String ->
    Maybe String -> IO ()
stopProgram nfe pf nf r = do
    atomically $ writeTChan (nfe^.funger.commIn) $
        KillProcess filt r
  where filt (pid, nam) = fromJust (checkPid pid <|> checkNam nam <|> Just True)
        checkPid pid = (pid ==) <$> pf
        checkNam nam = (nam ==) <$> nf

