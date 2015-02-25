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

data Exec w s m =
    Running (ProcessStateT w s m ())
  | Halted (Maybe String)
  | WBlock String Bool w (ProcessStateT w s m ())
  | RBlock String (w -> ProcessStateT w s m ())
  | PPID (PID -> ProcessStateT w s m ())
  | Fork (Bool -> ProcessStateT w s m ())
  | Rand (StdGen -> (ProcessStateT w s m (), StdGen))

type ProcessM w s m = ContT (Exec w s m) (StateT s m)

newtype ProcessStateT w s m a =
    PST { runPST :: (Exec w s m -> ProcessM w s m ()) -> ProcessM w s m a }

succeed :: ProcessM w s m a -> ProcessStateT w s m a
succeed m = PST $ \_ -> m

runPS :: Monad m => ProcessStateT w s m a -> s -> m (Exec w s m, s)
runPS (PST pst) s = runStateT (runContT runner return) s where
    runner = callCC $ \done -> do
        err <- callCC $ \trap -> do
            void $ pst trap
            done (Halted Nothing)
        return err

instance Functor m => Functor (ProcessStateT w s m) where
    fmap f (PST m) = PST $ \e -> fmap f (m e)

instance MonadTrans (ProcessStateT w s) where
    lift = succeed . lift . lift

instance MonadIO m => MonadIO (ProcessStateT w s m) where
    liftIO = succeed . liftIO

instance Monad m => Monad (ProcessStateT w s m) where
    return = succeed . return
    m >>= f = PST $ \e -> do
        a <- runPST m e
        runPST (f a) e

instance (Monoid w', MonadWriter w' m) =>
    MonadWriter w' (ProcessStateT w s m) where
        tell = succeed . lift . lift . tell
        listen m = m >>= succeed . lift . lift . listen . return
        pass m = m >>= succeed . lift . lift . pass . return

instance MonadReader r m => MonadReader r (ProcessStateT w s m) where
    ask = succeed . lift . lift $ ask
    local f (PST m) = PST $ \e -> liftLocal ask (mapStateT . local) f (m e)

instance Monad m => MonadState s (ProcessStateT w s m) where
    get = succeed . lift $ get
    put = succeed . lift . put

type PID = (Word32, String)

data Process w s m = Process {
    _procID    :: PID,
    _procExec  :: Exec w s m,
    _procState :: s
  }

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

data IDManager = IDMan Word32 [Word32]
    deriving (Show, Eq, Ord)

newID :: IDManager -> (Word32, IDManager)
newID (IDMan w (x:xs)) = (x, IDMan w xs)
newID (IDMan w []) = (w, IDMan (w+1) [])

freeID :: Word32 -> IDManager -> IDManager
freeID x (IDMan w xs) = IDMan w (x:xs)

instance Default IDManager where
    def = IDMan 0 []

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

kill :: Maybe String -> Process w s m -> Process w s m
kill reas proc = set procExec (Halted reas) proc

trap :: ((a -> ProcessStateT w s m b) -> Exec w s m) -> ProcessStateT w s m a
trap f = PST $ \e -> callCC $ \k -> do
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
getPID = trap $ PPID

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

advance :: (Monad m, Functor m) => VM w s m -> m (VM w s m)
advance vm = flip evalStateT (vm, mempty) $ initialize >> advanced where
    initialize = zoom _1 $ do
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
        res <- runMaybeT $ runQueued <|> runBuffers
        case res of
            Nothing -> do
                _1.buffers %= fmap (set bcastValues [])
                updateIDM
                pq <- use _2
                _1.processQueue .= pq
                use _1
            _ -> advanced
    updateIDM = do
        dead <- use (_1.deadProcesses)
        forM_ dead $ \((pid,_),_, _) -> do
            _1.idman %= freeID pid 

runStep :: Monad m => Process w s m -> m (Exec w s m, s)
runStep p = case (p^.procExec, p^.procState) of
    (Running f, s) -> runPS f s
    ex -> return ex

runQueued :: Monad m => MaybeT (StateT (VM w s m, Queue (Process w s m)) m) () 
runQueued = do
    q <- use (_1.processQueue)
    (p, q') <- MaybeT (return $ qPop q)
    _1.processQueue .= q'
    (ex, s) <- lift . lift $ runStep p
    let p' = set procExec ex $ set procState s $ p
        pid = p^.procID
    case ex of
        Running _ -> do
            _2 %= qPush p'
        Halted reas -> do
            _1.deadProcesses %= ((pid, reas, p'):)
        WBlock bname False _ _ -> do
            buf <- use (_1.buffers.(at bname))
            let buf' = maybe mempty id buf
                buf'' = buf' & writeQueue %~ qPush p'
            _1.buffers.(at bname) .= Just buf''
            return $ ()
        WBlock bname True w f -> do
            buf <- use (_1.buffers.(at bname))
            let buf' = maybe mempty id buf
                buf'' = buf' & bcastValues %~ (w:)
            _1.buffers.(at bname) .= Just buf''
            _1.processQueue %= qPush (set procExec (Running f) p')
        RBlock bname _ -> do
            buf <- use (_1.buffers.(at bname))
            let buf' = maybe mempty id buf
                buf'' = buf' & readQueue %~ qPush p'
            _1.buffers.(at bname) .= Just buf''
        Rand f -> do
            f' <- _1.gen %%= f
            _1.processQueue %= qPush (set procExec (Running f') p')
        PPID f -> do
            _1.processQueue %= qPush (set procExec (Running (f pid)) p')
        Fork f -> do
            _1.processQueue %= qPush (set procExec (Running (f False)) p')
            let prog = program (p^.procState) (f True)
            c <- _1 %%= makeProcess (p^.procID._2) prog
            _1.processQueue %= qPush c

runBuffers :: (Monad m) =>
    MaybeT (StateT (VM w s m, Queue (Process w s m)) m) ()
runBuffers = do
    bufs <- use (_1.buffers)
    let (bufs', ps) = runWriter . forM (M.toList bufs) $ \(bn, buf) -> do
            let (buf', p) = handleBuffer buf
            tell (p++)
            return (bn, buf')
    case (ps []) of
        [] -> fail ""
        ps' -> do
            _1.buffers .= M.fromList bufs'
            mapM_ ((_1.processQueue %=) .  qPush) ps'
            return ()

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

