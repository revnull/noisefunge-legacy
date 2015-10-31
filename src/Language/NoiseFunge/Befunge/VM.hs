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

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

module Language.NoiseFunge.Befunge.VM (VM(..), ProcessStateT,
                                       processQueue, buffers, deadProcesses,
                                       idman, newVM,
                                       Process, procID,
                                       Buffer, addProcess, Program,
                                       readQueue, writeQueue,
                                       PID, kill,
                                       ExecStats(..),
                                       VMStats(..), vmStats,
                                       vmPID, vmExec, vmMisc, 
                                       yield, readBuf, writeBuf, end, die,
                                       getPID, bcastBuf, fork, rand,
                                       getTime,
                                       getProcessState,
                                       program, advance) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont (liftLocal)
import Control.Monad.Trans.Maybe
import Control.Monad.Writer

import qualified Data.Binary as B
import Data.Default
import qualified Data.Map as M
import Data.Word

import System.Random

import Language.NoiseFunge.Beat

data Queue a = Q [a] [a]

qPush :: a -> Queue a -> Queue a
qPush a (Q xs ys) = Q xs (a:ys)

qPop :: Queue a -> Maybe (a, Queue a)
qPop (Q [] []) = Nothing
qPop (Q (x:xs) ys) = Just (x, Q xs ys)
qPop (Q [] ys) = qPop (Q ys [])

queued :: Lens (Queue a) (Queue b) [a] [b]
queued = lens gettr settr where
    gettr (Q x y) = x <> y
    settr (Q _ _) x = Q x []

instance Monoid (Queue a) where
    mempty = Q [] []
    mappend (Q x1 y1) (Q x2 y2) = Q x1 (y2 ++ (reverse x2) ++ y1)

instance Functor Queue where
    fmap f (Q x y) = Q (fmap f x) (fmap f y)

-- Exec is the running state of a Process at any guven time
data Exec w s m =
    Running (ProcessStateT w s m ()) -- yields
  | Halted (Maybe String) -- terminates
  | WBlock String Bool w (ProcessStateT w s m ()) -- blocking write
  | RBlock String (w -> ProcessStateT w s m ()) -- blocking read
  | PPID (PID -> ProcessStateT w s m ()) -- get PID from VM
  | Fork (Bool -> ProcessStateT w s m ()) -- fork a new microthread
  | Rand (StdGen -> (ProcessStateT w s m (), StdGen)) -- random
  | Time (Beat -> ProcessStateT w s m ()) -- get the current beat

-- A ProcessM is a continuation monad that returns an Exec over the state of
-- a process.
type ProcessM w s m = ContT (Exec w s m) (StateT s m)

-- A Trap function takes an Exec and returns a ProcessM. This is ultimately
-- used with callCC in a ProcessStateT to allow the process to yield, block,
-- or receive data from the VM.
type Trap w s m = Exec w s m -> ProcessM w s m ()

-- A ProcessStateT is a ProcessM with a reader over it for its Trap.
newtype ProcessStateT w s m a = PST {
    runPST :: ReaderT (Trap w s m) (ProcessM w s m) a }

-- succeed lifts a ProcessM into a ProcessStateT
succeed :: ProcessM w s m a -> ProcessStateT w s m a
succeed m = PST $ lift m

-- run a ProcessStateT for the given state. Return an Exec and the updated
-- state.
runPS :: Monad m => ProcessStateT w s m a -> s -> m (Exec w s m, s)
runPS (PST pst) s = runStateT (runContT runner return) s where
    runner = callCC $ \trap -> do
        void $ runReaderT pst trap
        return (Halted Nothing)

instance Functor m => Functor (ProcessStateT w s m) where
    fmap f (PST m) = PST $ fmap f m

instance MonadTrans (ProcessStateT w s) where
    lift = succeed . lift . lift

instance MonadIO m => MonadIO (ProcessStateT w s m) where
    liftIO = succeed . liftIO

instance Monad m => Monad (ProcessStateT w s m) where
    return = succeed . return
    m >>= f = PST $ do
        a <- runPST m
        runPST (f a)

instance (Monoid w', MonadWriter w' m) =>
    MonadWriter w' (ProcessStateT w s m) where
        tell = succeed . lift . lift . tell
        listen m = m >>= succeed . lift . lift . listen . return
        pass m = m >>= succeed . lift . lift . pass . return

instance MonadReader r m => MonadReader r (ProcessStateT w s m) where
    ask = succeed . lift . lift $ ask
    local f (PST m) = PST $ mapReaderT (liftLocal ask (mapStateT . local) f) m

instance Monad m => MonadState s (ProcessStateT w s m) where
    get = succeed . lift $ get
    put = succeed . lift . put

type PID = (Word32, String)

-- A Process is a running microthread in noisefunge. It has a PID, an ExecState
-- and its own state.
data Process w s m = Process {
    _procID    :: !PID,
    _procExec  :: Exec w s m,
    _procState :: s
  }

-- Buffers are used for IPC via input/output channels.
data Buffer w s m = Buffer {
    _readQueue   :: Queue (Process w s m),
    _writeQueue  :: Queue (Process w s m),
    _bcastValues :: [w]
  }

instance Monoid (Buffer w s m) where
    mempty = Buffer mempty mempty mempty
    mappend (Buffer r1 w1 b1) (Buffer r2 w2 b2) =
        Buffer (r1 <> r2) (w1 <> w2) (b1 <> b2)

instance Default (Buffer w s m) where
    def = mempty

-- IDManager is used to prevent duplication of the PID numbers
data IDManager = IDMan !Word32 [Word32]
    deriving (Show, Eq, Ord)

newID :: IDManager -> (Word32, IDManager)
newID (IDMan w (x:xs)) = (x, IDMan w xs)
newID (IDMan w []) = (w, IDMan (w+1) [])

freeID :: Word32 -> IDManager -> IDManager
freeID x (IDMan w xs) = IDMan w (x:xs)

instance Default IDManager where
    def = IDMan 0 []

-- VM is kind of a misleading name. This isn't a virtual machine as much as the
-- main data structure that contains the state of all of the processes and
-- output buffers at any given point in time.
data VM w s m = VM {
    _processQueue :: Queue (Process w s m),
    _buffers :: M.Map String (Buffer w s m),
    _deadProcesses :: [(PID, Maybe String, Process w s m)],
    _idman :: IDManager,
    _gen :: StdGen
  } 

instance Default (VM w s m) where
    def = VM mempty mempty mempty def (mkStdGen 1)

newVM :: IO (VM w s m)
newVM = VM mempty mempty mempty def <$> newStdGen

$(makeLenses ''VM)
$(makeLenses ''Process)
$(makeLenses ''Buffer)

-- To kill a process, set the state to Halted with an optional reason.
kill :: Maybe String -> Process w s m -> Process w s m
kill reas proc = set procExec (Halted reas) proc

-- trap is an operation in the ProcessStateT monad that takes a function which
-- converts the current continuation into an Exec. It then uses callCC to 
trap :: ((a -> ProcessStateT w s m b) -> Exec w s m) -> ProcessStateT w s m a
trap f = PST $ do
    e <- ask
    lift $ callCC $ \k -> do
        let ex = f (succeed . k)
        e ex
        undefined -- We will never reach this line

yield :: ProcessStateT w s m ()
yield = trap $ \k -> Running (k ())

fork :: ProcessStateT w s m Bool
fork = trap Fork

readBuf :: String -> ProcessStateT w s m w
readBuf buf = trap $ RBlock buf

writeBuf :: String -> w -> ProcessStateT w s m ()
writeBuf buf w = trap $ \k -> WBlock buf False w (k ())

bcastBuf :: String -> w -> ProcessStateT w s m ()
bcastBuf buf w = trap $ \k -> WBlock buf True w (k ())

rand :: Random a => (StdGen -> (a, StdGen)) -> ProcessStateT w s m a
rand fn = trap $ \k -> Rand $ \g ->
    let (a, g') = fn g
    in (k a, g')

end :: ProcessStateT w s m a
end = trap (const (Halted Nothing))

die :: String -> ProcessStateT w s m a
die s = trap (const (Halted (Just s)))

getPID :: ProcessStateT w s m PID
getPID = trap PPID

getTime :: ProcessStateT w s m Beat
getTime = trap Time

getProcessState :: Monad m => ProcessStateT w s m s
getProcessState = succeed . lift $ get

type Program w s m = PID -> Process w s m

program :: Monad m => s -> ProcessStateT w s m a -> Program w s m
program s pst pid = Process pid (Running $ pst >> end) s

makeProcess :: String -> Program w s m -> VM w s m -> (Process w s m, VM w s m)
makeProcess name f vm = (f pid, set idman ids vm) where
    pid = (w, name)
    (w, ids) = newID (vm^.idman)

queueProcess :: Process w s m -> VM w s m -> VM w s m
queueProcess p = processQueue %~ qPush p
    
addProcess :: String -> Program w s m -> VM w s m -> (Process w s m, VM w s m)
addProcess name f vm = (p, queueProcess p vm') where
    (p, vm') = makeProcess name f vm


-- These lenses are meant to be used in the StateT monad in the advance
-- function. They are meant to give semantic meaning to commonly used
-- lenses.

currentVM :: Simple Lens (VM w s m, Queue (Process w s m)) (VM w s m)
currentVM = _1

currentQueue :: Simple Lens (VM w s m, Queue (Process w s m))
    (Queue (Process w s m))
currentQueue = _1.processQueue

doneQueue :: Simple Lens (VM w s m, Queue (Process w s m))
    (Queue (Process w s m))
doneQueue = _2


-- advance the VM. This takes the current Beat and VM and returns the VM state
-- after a single tick of the clock. A tick runs all non-blocked tasklets until
-- they yield or are blocked.
--
-- The state in the StateT monad is a tuple of the VM and queue of processes
-- that have finished their tick.
advance :: (Monad m, Functor m) => Beat -> VM w s m -> m (VM w s m)
advance bt vm = flip evalStateT (vm, mempty) $ initialize >> advanced where
    initialize = zoom currentVM $ do
        -- Clean up the killed processes
        killed <- execWriterT $ do
            zoom processQueue filterQueue
            zoom (buffers.traverse) $ do
                zoom readQueue filterQueue
                zoom writeQueue filterQueue
        deadProcesses .= killed []
    filterQueue = do
        qd <- use queued
        qd' <- filterM filterDead qd
        queued .= qd'
    filterDead p = case (p^.procExec) of
        (Halted r) -> do
            tell ((p^.procID, r, p):)
            return False
        _ -> return True
    advanced = do
        -- run a queued process if one exits. Otherwise run a buffered
        -- process.
        res <- runMaybeT $ runQueued bt <|> runBuffers
        case res of
            Nothing -> do -- Nothing (left to) do
                currentVM.buffers %= fmap (set bcastValues [])
                updateIDM

                -- replace the processQueue with the finished processes
                pq <- use doneQueue
                currentQueue .= pq
                use currentVM
            _ -> advanced -- still work to do
    -- free the IDs of the dead processes
    updateIDM = do
        dead <- use (currentVM.deadProcesses)
        forM_ dead $ \((pid,_),_, _) -> do
            currentVM.idman %= freeID pid 

runStep :: Monad m => Process w s m -> m (Exec w s m, s)
runStep p = case (p^.procExec, p^.procState) of
    (Running f, s) -> runPS f s
    ex -> return ex

-- run a Queued process, if there are any left on the queue.
runQueued :: Monad m => Beat ->
    MaybeT (StateT (VM w s m, Queue (Process w s m)) m) () 
runQueued bt = do
    q <- use (currentQueue)
    (p, q') <- MaybeT (return $ qPop q)
    currentQueue .= q'
    (ex, s) <- lift . lift $ runStep p
    let p' = set procExec ex $ set procState s $ p
        pid = p^.procID
    case ex of
        -- process yielded
        Running _ -> do
            -- onto the finished (for this tick) queue
            doneQueue %= qPush p'
        
        --process halted
        Halted reas -> do
            currentVM.deadProcesses %= ((pid, reas, p'):)

        -- process ends up blocked on a write buffer
        WBlock bname False _ _ -> do -- 
            buf <- use (currentVM.buffers.(at bname))
            let buf' = maybe mempty id buf
                buf'' = buf' & writeQueue %~ qPush p'
            currentVM.buffers.(at bname) .= Just buf''
            return $ ()

        -- process broadcasts a value to a write buffer and continues
        WBlock bname True w f -> do
            buf <- use (currentVM.buffers.(at bname))
            let buf' = maybe mempty id buf
                buf'' = buf' & bcastValues %~ (w:)
            currentVM.buffers.(at bname) .= Just buf''
            -- back onto the active queue for this tick
            currentQueue %= qPush (set procExec (Running f) p')

        -- block on the read queue
        RBlock bname _ -> do
            buf <- use (currentVM.buffers.(at bname))
            let buf' = maybe mempty id buf
                buf'' = buf' & readQueue %~ qPush p'
            currentVM.buffers.(at bname) .= Just buf''

        -- run the random function. Updating the StdGen of the VM.
        Rand f -> do
            f' <- currentVM.gen %%= f
            -- back onto the active queue for this tick
            currentQueue %= qPush (set procExec (Running f') p')

        -- process requests its PID
        PPID f -> do
            -- back onto the active queue for this tick
            currentQueue %= qPush (set procExec (Running (f pid)) p')

        -- process wants to fork
        Fork f -> do
            currentQueue %= qPush (set procExec (Running (f False)) p')
            let prog = program (p^.procState) (f True)
            -- make a new process based on the Exec of the current proces.
            -- (automatically queued)
            c <- currentVM %%= makeProcess (p^.procID._2) prog
            -- back onto the active queue for this tick
            currentQueue %= qPush c

        -- process wants the current beat.
        Time f -> do
            -- back onto the active queue for this tick
            currentQueue %= qPush (set procExec (Running (f bt)) p')

-- Iterate through the buffers and run the next process that can read or
-- write from a buffer.
runBuffers :: (Monad m) =>
    MaybeT (StateT (VM w s m, Queue (Process w s m)) m) ()
runBuffers = do
    bufs <- use (currentVM.buffers)
    let (bufs', ps) = runWriter . forM (M.toList bufs) $ \(bn, buf) -> do
            let (buf', p) = handleBuffer buf
            tell (p++)
            return (bn, buf')
    case (ps []) of
        [] -> fail "" -- no processes freed by the buffers
        ps' -> do
            -- update the buffers
            currentVM.buffers .= M.fromList bufs'
            -- add the unblocked processes to the current running queue
            mapM_ ((currentQueue %=) .  qPush) ps'
            return ()

-- Run through a buffer and return a list of any Processes that are no longer
-- blocked on the buffer.
handleBuffer :: Buffer w s m -> (Buffer w s m, [Process w s m])
handleBuffer buf = (buf'', concatMap (uncurry comm) ps) where
    (ps, buf') = pairs (cycleBC buf)
    buf'' = set bcastValues (buf^.bcastValues) buf'
    comm rp (Right wp) =
        let (WBlock _ _ w wres) = wp^.procExec
            (RBlock _ rres) = rp^.procExec
        in [set procExec (Running (rres w)) rp,
            set procExec (Running wres) wp]
    comm rp (Left w) =
        let (RBlock _ rres) = rp^.procExec
        in [set procExec (Running (rres w)) rp]
    -- multiple broadcasts cycle among eachother.
    cycleBC b@(Buffer _ _ []) = b
    cycleBC (Buffer rq wq l) = Buffer rq wq (cycle l)
    pair b = pairQ b <|> pairBC b
    pairQ b = do
        (r, rq) <- qPop (b^.readQueue)
        (w, wq) <- qPop (b^.writeQueue)
        return (r, Right w, Buffer rq wq (b^.bcastValues))
    pairBC b = do
        (r, rq) <- qPop (b^.readQueue)
        (w, ws) <- uncons (b^.bcastValues)
        return (r, Left w, Buffer rq (b^.writeQueue) ws)
    pairs b = case pair b of
        Nothing -> ([], set bcastValues [] b)
        Just (r, w, b') ->
            let (ps', b'') = pairs b'
            in ((r,w):ps', b'')

data ExecStats =
    ERunning
  | EHalted (Maybe String)
  | ERBlock String
  | EWBlock String
  deriving (Read, Show, Eq, Ord)

exStats :: Exec w s m -> ExecStats
exStats (Halted r) = EHalted r
exStats (WBlock b _ _ _) = EWBlock b
exStats (RBlock b _) = ERBlock b
exStats _ = ERunning

instance B.Binary ExecStats where
    get = B.getWord8 >>= getEx where
        getEx 0 = return ERunning
        getEx 1 = EHalted <$> B.get
        getEx 2 = ERBlock <$> B.get
        getEx 3 = EWBlock <$> B.get
        getEx _ = error "Bad ExecStats"
    put ERunning = B.putWord8 0
    put (EHalted a) = B.putWord8 1 >> B.put a
    put (ERBlock a) = B.putWord8 2 >> B.put a
    put (EWBlock a) = B.putWord8 3 >> B.put a

data VMStats s = VMStats {
    _vmPID  :: PID,
    _vmExec :: ExecStats,
    _vmMisc :: s
  } deriving (Read, Show, Eq, Ord)

$(makeLenses ''VMStats)

instance Functor VMStats where
    fmap f (VMStats pid ex a) = VMStats pid ex (f a)

vmStats :: Getter (VM w s m) [VMStats s]
vmStats = to vmStats' where
    vmStats' vm =
        let running = do
                p <- vm^.processQueue.queued
                return $ VMStats (p^.procID) (p^.procExec.(to exStats))
                    (p^.procState)
            blocked = do
                buf <- vm^.buffers.(to M.elems)
                p <- (buf^.readQueue.queued) <> (buf^.writeQueue.queued)
                return $ VMStats (p^.procID) (p^.procExec.(to exStats))
                    (p^.procState)
            halted = do
                (_,_,p) <- vm^.deadProcesses
                return $ VMStats (p^.procID) (p^.procExec.(to exStats))
                    (p^.procState)
        in running <> blocked <> halted

instance B.Binary a => B.Binary (VMStats a) where
    get = VMStats <$> B.get <*> B.get <*> B.get
    put (VMStats a b c) = B.put a >> B.put b >> B.put c

