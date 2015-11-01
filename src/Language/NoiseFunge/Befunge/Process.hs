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

module Language.NoiseFunge.Befunge.Process (ProgArray, makeProgArray,
                                            Dir(..), PC(PC),
                                            Event(..),
                                            pos, dir, Pos,
                                            Delta(..), oldpc, newpc,
                                            change, events,
                                            ProcessState, makeProcessState,
                                            Stack, pop, (#+), stackLength,
                                            mem, pc, stack, quote,
                                            jump, progIn, progOut,
                                            fnStack,
                                            noteBuf, ticks,
                                            OperatorParams(..),
                                            Fungine, FungeVM,
                                            FungeProcess, FungeProgram,
                                            Deltas, ProcessStats,
                                            psTicks, psStackSize,
                                            psQuote,
                                            Operator(..),
                                            opCode, opChar,
                                            opName, opDesc,
                                            OpSet(..),
                                            processStats,
                                            tellDelta, tellMem
                                            ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.RWS

import qualified Data.Array as Arr
import Data.Array.Unboxed
import qualified Data.Binary as B
import Data.Char
import Data.Default
import Data.Word

import Language.NoiseFunge.Befunge.VM
import Language.NoiseFunge.Note

-- This module builds upon the VM code to generate processes that are distinct
-- to the noisefunge engine. This provides the underlying functionality that
-- the operators are built around. It includes things such as the program
-- counter, stack, and process memory.

type Pos = (Word8, Word8)

type ProgArray = UArray Pos Word8

-- Convert a list of lines into a program array. The height will be the number
-- of lines and the width will be the length of the longest line.
makeProgArray :: [String] -> ProgArray
makeProgArray strs = arr where
    rows = fromIntegral $ length strs
    cols = fromIntegral $ maximum $ fmap length strs
    bnds = ((0,0), (rows-1, cols-1))
    arr = array bnds $ do
        (r, row) <- zip [0..] strs
        (c, char) <- zip [0..cols-1] (row ++ pad) 
        return ((r,c), fromIntegral $ ord char)
    pad = repeat ' '

data Dir = U | D | L | R
    deriving (Read, Show, Eq, Ord, Enum)

instance B.Binary Dir where
    get = (toEnum . fromIntegral) <$> B.getWord8
    put = B.putWord8 . fromIntegral . fromEnum 

data PC = PC {
    _pos :: !Pos,
    _dir :: !Dir
    } deriving (Read, Show, Eq, Ord)

instance Default PC where
    def = PC (0,0) R

instance B.Binary PC where
    get = PC <$> B.get <*> B.get
    put (PC p d) = B.put p >> B.put d

data Event =
    StringEvent String
  | ErrorEvent String
  | NoteEvent !Note
  deriving (Read, Show, Eq, Ord)

instance B.Binary Event where
    get = B.getWord8 >>= getEv where
        getEv 0 = StringEvent <$> B.get
        getEv 1 = ErrorEvent <$> B.get
        getEv 2 = NoteEvent <$> B.get
        getEv _ = error "Bad event"
    put (StringEvent s) = B.putWord8 0 >> B.put s
    put (ErrorEvent s) = B.putWord8 1 >> B.put s
    put (NoteEvent n) = B.putWord8 2 >> B.put n

-- A delta expresses how a process has changed in the last tick.
data Delta = Delta {
    _oldpc  :: Maybe PC,
    _newpc  :: Maybe PC,
    _change :: Maybe ProgArray,
    _events :: [Event]
    } deriving (Show, Eq, Ord)

instance Monoid Delta where
    mempty = Delta Nothing Nothing Nothing []
    mappend (Delta op1 np1 c1 e1) (Delta op2 np2 c2 e2) =
        Delta (op1 `mplus` op2) (np2 `mplus` np1)
            (c2 `mplus` c1) (e1 `mappend` e2)

instance B.Binary Delta where
    get = Delta <$> B.get <*> B.get <*> B.get <*> B.get
    put (Delta a b c d) = B.put a >> B.put b >> B.put c >> B.put d

instance Default Delta where
    def = mempty

type Deltas s = [(PID, s, Delta)] -> [(PID, s, Delta)]

data Stack a = Stack !Word32 [a]
    deriving (Read, Show, Eq, Ord)

instance Monoid (Stack a) where
    mempty = Stack 0 []
    mappend (Stack xl xs) (Stack yl ys) = Stack (xl + yl) (xs ++ ys)

(#+) :: a -> Stack a -> Stack a
a #+ Stack l xs = Stack (l+1) (a:xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack 0 []) = Nothing
pop (Stack 0 _) = error "Invalid stack length"
pop (Stack _ []) = error "Invalid stack contents"
pop (Stack l (x:xs)) = Just (x, Stack (l-1) xs)

stackLength :: Stack a -> Word32
stackLength (Stack l _) = l

data ProcessState = PS {
    _mem     :: !ProgArray,
    _ticks   :: !Word32,
    _pc      :: !PC,
    _stack   :: !(Stack Word8),
    _quote   :: !Bool,
    _jump    :: !Bool,
    _fnStack :: !(Maybe [Word8]),
    _progIn  :: !String, -- Name of input buffer
    _progOut :: !String, -- Name of output buffer
    _noteBuf :: !(Maybe Note)
    }

makeProcessState :: ProgArray -> String -> String -> ProcessState
makeProcessState arr inp outp =
    PS arr 0 def mempty False False mempty inp outp Nothing

-- OperatorParams provides some options for how noisefunge operators should
-- behave.
data OperatorParams = OperatorParams {
    haltOnError :: !Bool,
    wrapOnEdge :: !Bool,
    debugLogging :: !Bool
  } deriving (Read, Show, Eq, Ord)

instance Default OperatorParams where
    def = OperatorParams True False False

data Operator = Operator {
    _opName :: String,
    _opChar :: Char,
    _opDesc :: String,
    _opCode :: (Fungine ())
  }

newtype OpSet = OpSet { getOpSet :: Arr.Array Word8 (Maybe (Fungine ())) }

type FungeRWS = RWS OperatorParams (Deltas ProcessState) OpSet

type Fungine = ProcessStateT Word8 ProcessState FungeRWS

type FungeVM = VM Word8 ProcessState FungeRWS

type FungeProcess = Process Word8 ProcessState FungeRWS

type FungeProgram = Program Word8 ProcessState FungeRWS

$(makeLenses ''ProcessState)
$(makeLenses ''Delta)
$(makeLenses ''PC)
$(makeLenses ''Operator)

tellDelta :: Delta -> Fungine ()
tellDelta d = do
    pid <- getPID
    st <- getProcessState
    tell ((pid, st, d):)

tellMem :: Fungine ()
tellMem = do
    arr <- use mem
    tellDelta $ Delta Nothing Nothing (Just arr) []

data ProcessStats = PStats {
    _psTicks     :: !Word32,
    _psStackSize :: !Word32,
    _psQuote     :: !Bool
  } deriving (Show, Eq, Ord)

$(makeLenses ''ProcessStats)

instance B.Binary ProcessStats where
    get = PStats <$> B.get <*> B.get <*> B.get
    put (PStats a b c) = B.put a >> B.put b >> B.put c

processStats :: Getter ProcessState ProcessStats
processStats = to processStats' where
    processStats' ps = PStats (ps^.ticks) (stackLength $ ps^.stack)
        (ps^.quote )

