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
module Language.NoiseFunge.ALSA (startALSAThread, ALSAThread,
                                 tid, clock, inEvents,
                                 outEvents) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad

import Data.Array
import Data.Default
import Data.Ratio
import Data.Word

import qualified Sound.ALSA.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.InfoMonad as PortInfo
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer.RealTime as RealTime

import Language.NoiseFunge.Beat
import Language.NoiseFunge.Note

import System.Environment

data ALSAThread = ALSAThread {
    _tid       :: ThreadId,
    _clock     :: TVar Beat,
    _inEvents  :: TChan (Beat, Note),
    _outEvents :: TChan (Beat, Note)
    }

$(makeLenses ''ALSAThread)

beatTime :: Tempo -> Beat -> RealTime.T
beatTime (Tempo tb ts) (Beat b s) = rt where
    perbeat :: Ratio Integer
    perbeat = 60 % (fromIntegral $ tb * ts)
    rt = RealTime.fromFractional (perbeat * bt)
    bt = (fromIntegral $ b * ts + s) % 1

startALSAThread :: Tempo -> IO ALSAThread
startALSAThread tempo = do
    cl  <- newTVarIO def
    nin <- newTChanIO
    nout <- newTChanIO
    let handler = alsaHandler tempo cl nin nout
    th <- forkIO $ Seq.withDefault Seq.Block handler
    return $ ALSAThread th cl nin nout

alsaHandler :: Tempo -> TVar Beat -> TChan (Beat, Note) -> TChan (Beat, Note)
    -> (Seq.T Seq.DuplexMode) -> IO ()
alsaHandler tempo@(Tempo tb ts) clockv _ noteout h = do 
    getProgName >>= Client.setName h

    c <- Client.getId h
    q <- Queue.alloc h
    ioaddrs <- (listArray (0,15) <$>) . forM [0..15] $ \i -> do
        let name = "channels " ++ show minb ++ "-" ++ show maxb
            minb :: Word8
            minb = i * 16
            maxb = minb + 15
        ioport <- Port.createSimple h name
            (Port.caps [Port.capRead, Port.capSubsRead])
            (Port.types [Port.typeMidiGeneric, Port.typeApplication])
        PortInfo.modify h ioport $ do
            PortInfo.setTimestamping True
            PortInfo.setTimestampReal True
            PortInfo.setTimestampQueue q
        return $ Addr.Cons c ioport

    priv <- Port.createSimple h "priv"
        (Port.caps [Port.capRead, Port.capWrite])
        (Port.types [Port.typeMidiGeneric])
    PortInfo.modify h priv $ do
        PortInfo.setTimestamping True
        PortInfo.setTimestampReal True
        PortInfo.setTimestampQueue q

    let praddr   = Addr.Cons c priv
        ticktime = fromIntegral ((3000000000 :: Integer) `div`
                                 (fromIntegral (tb * ts)))
        timefn   = Time.consAbs . Time.Real . beatTime tempo

    Queue.control h q (Event.QueueTempo (Event.Tempo ticktime)) Nothing
    Queue.control h q Event.QueueStart Nothing

    let echo bt@(Beat b s) = void . Event.output h $ (Event.simple praddr
            (Event.CustomEv Event.Echo $ Event.Custom b s 0)) {
                Event.time = timefn bt,
                Event.dest = praddr,
                Event.queue = q
            }
    let play addr b ev = void . Event.output h $ (Event.simple addr ev) {
                Event.time = timefn b,
                Event.dest = Addr.subscribers,
                Event.queue = q
            }
    let step = (tempo ##)

    let loopEvents = do
            notes <- atomically $ drainTChan noteout
            forM_ notes $ \(b@(Beat x y), n) -> do
                let ne = Event.simpleNote (Event.Channel chan)
                        (Event.Pitch $ n^.pitch) (Event.Velocity $ n^.velocity)
                    (port, chan) = (n^.channel) `divMod` 16
                    nton = Event.NoteEv Event.NoteOn ne
                    ntof = Event.NoteEv Event.NoteOff ne
                    addr = ioaddrs ! port
                play addr b nton
                play addr (Beat (x + fromIntegral (n^.duration)) y) ntof
            _ <- Event.drainOutput h
            event <- Event.input h
            case Event.body event of
                Event.CustomEv Event.Echo (Event.Custom b s 0) -> do
                    let next = step curr
                        curr = (Beat b s)
                    atomically $ writeTVar clockv curr
                    echo next
                    return ()
                _ -> return ()
            loopEvents
    echo def
    _ <- Event.outputPending h
    loopEvents

drainTChan :: TChan a -> STM [a]
drainTChan c = drainer where
    drainer = do
        empt <- isEmptyTChan c
        if empt then return [] else (:) <$> readTChan c <*> drainer

