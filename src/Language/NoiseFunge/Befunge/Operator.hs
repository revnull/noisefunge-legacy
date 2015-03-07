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

{-# LANGUAGE RankNTypes #-}

module Language.NoiseFunge.Befunge.Operator (OperatorParams(..),
                                             move, getOp, runOp, quoteOp,
                                             fnStackOp,
                                             stdOps,
                                             tellMem, operators,
                                             logError, logDebug
                                             ) where

import Control.Applicative
import Control.Lens
import Control.Monad.RWS

import qualified Data.Array as Arr
import Data.Array.Unboxed
import Data.Char
import Data.Default
import qualified Data.Map as M
import Data.Word

import System.Random

import Language.NoiseFunge.Befunge.Process
import Language.NoiseFunge.Befunge.VM
import Language.NoiseFunge.Note
import Language.NoiseFunge.Beat

boundedStep :: (Word8, Word8) -> Int -> PC -> Either Pos Pos
boundedStep (_, xm) s (PC (y, x) L) =
    let x' = fromIntegral x - s
        ov = fromIntegral (fromIntegral xm + 1 + x')
    in if x' < 0 then Left (y, ov) else Right (y, fromIntegral x')
boundedStep (_, xm) s (PC (y, x) R) =
    let x' = fromIntegral x + s
        ov = fromIntegral (x' - fromIntegral xm - 1)
    in if x' > fromIntegral xm then Left (y, ov) else Right (y, fromIntegral x')
boundedStep (ym, _) s (PC (y, x) U) =
    let y' = fromIntegral y - s
        ov = fromIntegral (fromIntegral ym + 1 + y')
    in if y' < 0 then Left (ov, x) else Right (fromIntegral y', x)
boundedStep (ym, _) s (PC (y, x) D) =
    let y' = fromIntegral y + s
        ov = fromIntegral (y' - fromIntegral ym - 1)
    in if y' > fromIntegral ym then Left (ov, x) else Right (fromIntegral y', x)

logDebug :: String -> Fungine ()
logDebug str = do
    deb <- asks debugLogging
    when deb . tell $ def { _events = [ErrorEvent str] }

logError :: String -> Fungine ()
logError str = tell $ def { _events = [ErrorEvent str] }

dieError :: String -> a -> Fungine a
dieError str a = do
    hal <- asks haltOnError
    logError str
    if hal
        then lift (die str) >> return a
        else return a

move :: Fungine ()
move = do
    j <- use jump
    sd <- if j
        then jump .= False >> return 2
        else return 1
    (_,bnds) <- bounds `fmap` use mem
    c@(PC _ d) <- use pc
    hal <- asks haltOnEdge
    let p' = boundedStep bnds sd c
    p'' <- case (hal, p') of
        (False, Left p'') -> return p''
        (False, Right p'') -> return p''
        (True, Right p'') -> return p''
        (True, Left p'') -> dieError "Exceeded memory bounds" p''
    pc.pos .= p''
    tell $ def { _oldpc = Just c, _newpc = Just (PC p'' d) }

tellMem :: Fungine ()
tellMem = do
    arr <- use mem
    tell $ Delta Nothing Nothing (Just arr) []

quoteOp :: Word8 -> Fungine ()
quoteOp 0x22 = quote .= False
quoteOp x = pushOp x

fnStackOp :: Word8 -> Fungine ()
fnStackOp 0x5d = do
    Just ops <- use fnStack
    OpSet arr <- lift . lift $ get
    case ops of
        (o:os) -> do
            let fun = foldr proc (Just $ return ()) os
                proc c n = do
                    oper <- arr ! c
                    n' <- n
                    return $ oper >> n'
            case fun of
                Nothing -> dieError "Invalid opcode in function." ()
                Just _ -> lift . lift . put . OpSet $ (arr // [(o, fun)])
        [] -> dieError "Empty function definition." ()
    fnStack .= Nothing
fnStackOp x = fnStack %= (fmap (x:))

pushOp :: Word8 -> Fungine ()
pushOp ch = do
    st <- use stack
    let sl = stackLength st
    when (sl > 1024) $ do
        dieError ("Large stack size: " ++ show sl) ()
    stack %= (ch #+)

popOp :: Fungine Word8
popOp = do
    st <- use stack
    case pop st of
        Just (x, st') -> do
            stack .= st'
            return x
        Nothing -> do
            dieError "Pop from empty stack" 0

getOp :: Fungine Word8
getOp = do
    (PC (x,y) _) <- use pc
    let i = (fromIntegral x, fromIntegral y)
    (! i) `fmap` use mem

stdOps :: M.Map Word8 Operator
stdOps = M.fromList $ [
    mkStdOp "NOP" ' ' "Do Nothing" $ return ()
    , mkStdOp "Left" '<' "Change direction to left." $ do
        pc.dir .= L
    , mkStdOp "Right" '>' "Change direction to right." $ do
        pc.dir .= R
    , mkStdOp "Down" 'v' "Change direction to down." $ do
        pc.dir .= D
    , mkStdOp "Up" '^' "Change direction to up." $ do
        pc.dir .= U
    , mkStdOp "0" '0' "Push 0 onto the stack" $
        pushOp 0
    , mkStdOp "1" '1' "Push 1 onto the stack" $
        pushOp 1
    , mkStdOp "2" '2' "Push 2 onto the stack" $
        pushOp 2
    , mkStdOp "3" '3' "Push 3 onto the stack" $
        pushOp 3
    , mkStdOp "4" '4' "Push 4 onto the stack" $
        pushOp 4
    , mkStdOp "5" '5' "Push 5 onto the stack" $
        pushOp 5
    , mkStdOp "6" '6' "Push 6 onto the stack" $
        pushOp 6
    , mkStdOp "7" '7' "Push 7 onto the stack" $
        pushOp 7
    , mkStdOp "8" '8' "Push 8 onto the stack" $
        pushOp 8
    , mkStdOp "9" '9' "Push 9 onto the stack" $
        pushOp 9
    , mkStdOp "A" 'A' "Push 10 onto the stack" $
        pushOp 10
    , mkStdOp "B" 'B' "Push 11 onto the stack" $
        pushOp 11
    , mkStdOp "C" 'C' "Push 12 onto the stack" $
        pushOp 12
    , mkStdOp "D" 'D' "Push 13 onto the stack" $
        pushOp 13
    , mkStdOp "E" 'E' "Push 14 onto the stack" $
        pushOp 14
    , mkStdOp "F" 'F' "Push 15 onto the stack" $
        pushOp 15
    , mkStdOp "Fork" 'K' "Fork a thread. Push 1 for child, 0 for parent" $ do
        new <- lift $ fork
        if new
            then do
                ticks .= 0
                pushOp 1
                tellMem
            else pushOp 0
    , mkStdOp "Get" 'g' "Pop y and x. Push memory at (y,x)" $ do
        y <- popOp
        x <- popOp
        arr <- use mem
        pushOp (arr ! (y,x))
    , mkStdOp "Put" 'p' "Pop y, x, and v. Write v to (y,x)" $ do
        y <- popOp
        x <- popOp
        v <- popOp
        arr <- use mem
        let arr' = arr // [((y,x),v)]
        mem .= arr'
        tellMem
    , mkStdOp "Call" 'c' "Pop y and x. Run opcode at (y,x)" $ do
        y <- popOp
        x <- popOp
        arr <- use mem
        runOp (arr ! (y,x))
    , mkStdOp "Quantize" 'q' "Wait for the next beat" $ do
        let quant = do
                bt <- getTime
                if (bt^.subbeat) == 0
                    then return ()
                    else yield >> quant
        lift quant
    , mkStdOp "QuantizeN" 'Q'
        "Pop x. Wait for a beat that is divisible by x." $ do
        x <- fromIntegral <$> popOp
        let quant = do
                bt <- getTime
                if (bt^.subbeat) == 0 && (bt^.beat) `mod` x == 0
                    then return ()
                    else yield >> quant
        lift quant
    , mkStdOp "Sleep" 's' "Pop x. Sleep for x subbeats." $ do
        x <- popOp
        forM_ [1..x] $ const (lift yield)
    , mkStdOp "Swap" '\\' "Swap the top two items on the stack" $ do
        a <- popOp
        b <- popOp
        pushOp a
        pushOp b
    , mkStdOp "Chomp" '$' "Discard the top item on the stack" $ do
        void popOp
    , mkStdOp "Jump" '#' "Jump over the next opcode." $ do
        jump .= True
    , mkStdOp "Quote" '"' "Start/Stop quote mode." $ do       
        quote .= True
    , mkStdOp "Defun" '[' "Start a function definition." $ do
        fnStack .= Just []
    , mkStdOp "Defun(end)" ']' "End a function definition." $ do
        dieError "Not defining a function." ()
    , mkStdOp "Dup" ':' "Duplicate the top object on the stack." $ do
        a <- popOp
        pushOp a
        pushOp a
    , mkStdOp "Cond(V)" '|' "Pop x. If x is 0, go down, otherwise go up." $ do       
        a <- popOp
        if a == 0
            then pc.dir .= D
            else pc.dir .= U
    , mkStdOp "Cond(H)" '_' "Pop x. If x is 0, go right, else left." $ do
        a <- popOp
        if a == 0
            then pc.dir .= R
            else pc.dir .= L
    , mkStdOp "Cond(Jump)" '\'' "Pop x. If x is 0, jump." $ do
        a <- popOp
        when (a == 0) $ jump .= True
    , mkStdOp "Random(Dir)" '?' "Change to a random direction." $ do
        r <- lift . rand $ randomR (0, 3)
        let d = case (r :: Int) of
                0 -> L
                1 -> R
                2 -> U
                3 -> D
                _ -> error "Random failure in ?"
        pc.dir .= d
    , mkStdOp "Random(Byte)" 'r' "Push a random byte onto the stack." $ do
        r <- lift . rand $ random
        pushOp r
    , mkStdOp "Random(Range)" 'R'
        "Pop x and y. Push a random byte between x and y (inclusive)." $ do
        a <- popOp
        b <- popOp
        r <- lift . rand $ randomR (b, a)
        pushOp r
    , mkStdOp "Not" '!' "Pop x. If x is 0, push 1, else push 0." $ do
        a <- popOp
        if a == 0
            then pushOp 1
            else pushOp 0
    , mkStdOp "GT" '`' "Pop x and y. If y > x, push 1, else push 0." $ do
        a <- popOp
        b <- popOp
        if b > a
            then pushOp 1
            else pushOp 0
    , mkStdOp "Add" '+' "Pop x and y. Push y + x." $ do
        a <- popOp
        b <- popOp
        pushOp (a + b)
    , mkStdOp "Sub" '-' "Pop x and y. Push y - x." $ do
        a <- popOp
        b <- popOp
        pushOp (b - a)
    , mkStdOp "Mul" '*' "Pop x and y. Push y * x." $ do
        a <- popOp
        b <- popOp
        pushOp (a * b)
    , mkStdOp "Div" '/' "Pop x and y. Push y / x." $ do
        a <- popOp
        b <- popOp
        if a /= 0
            then pushOp (b `div` a)
            else do
                dieError "Divide by zero" 255 >>= pushOp
    , mkStdOp "Mod" '%' "Pop x and y. Push y % x." $ do
        a <- popOp
        b <- popOp
        if a /= 0
            then pushOp (b `mod` a)
            else do
                dieError "Modulo by zero" 0 >>= pushOp
    , mkStdOp "Output" '.' "Pop x. Write x to output buffer." $ do
        a <- popOp
        outbuf <- use progOut
        lift $ writeBuf outbuf a
    , mkStdOp "Broadcast" ';' "Pop x. Broadcast x to output buffer." $ do
        a <- popOp
        outbuf <- use progOut
        lift $ bcastBuf outbuf a
    , mkStdOp "Read" '~' "Read a value from input buffer and push it." $ do
        inbuf <- use progIn
        ch <- lift $ readBuf inbuf
        pushOp ch
    , mkStdOp "Print(Char)" ',' "Pop x. Print x as a character." $ do 
        a <- (chr . fromIntegral) `fmap` popOp
        tell $ def { _events = [StringEvent [a]] }
    , mkStdOp "Print(Byte)" '&' "Pop x. Print x as a number." $ do
        a <- popOp
        tell $ def { _events = [StringEvent $ show a] }
    , mkStdOp "Major" 'M' "Pop x. Play major chord of pitch x." $ do
        nb <- use noteBuf
        n <- case nb of
            Nothing -> dieError "Can't play empty note" def
            (Just n) -> return n
        let nes = do
                ch <- [0, 4, 7]
                return $ NoteEvent (pitch %~ (ch +) $ n)
        tell $ def { _events = nes }
    , mkStdOp "Minor" 'm' "Pop x. Play minor chord of pitch x." $ do
        nb <- use noteBuf
        n <- case nb of
            Nothing -> dieError "Can't play empty note" def
            (Just n) -> return n
        let nes = do
                ch <- [0, 3, 7]
                return $ NoteEvent (pitch %~ (ch +) $ n)
        tell $ def { _events = nes }
    , mkStdOp "Major7" 'L' "Pop x. Play major 7th chord of pitch x." $ do
        nb <- use noteBuf
        n <- case nb of
            Nothing -> dieError "Can't play empty note" def
            (Just n) -> return n
        let nes = do
                ch <- [0, 4, 7, 11]
                return $ NoteEvent (pitch %~ (ch +) $ n)
        tell $ def { _events = nes }
    , mkStdOp "Minor7" 'l' "Pop x. Play minor 7th chord of pitch x." $ do
        nb <- use noteBuf
        n <- case nb of
            Nothing -> dieError "Can't play empty note" def
            (Just n) -> return n
        let nes = do
                ch <- [0, 3, 7, 11]
                return $ NoteEvent (pitch %~ (ch +) $ n)
        tell $ def { _events = nes }
    , mkStdOp "Write(Note)" 'z' "Pop dur vel pch cha. Write note buffer." $ do
        dur <- popOp
        vel <- popOp
        pit <- popOp
        cha <- popOp
        noteBuf .= Just (Note cha pit vel dur)
    , mkStdOp "Play(Note)" 'Z' "Play the note in the note buffer." $ do
        nb <- use noteBuf
        ne <- case nb of
            Nothing -> dieError "Can't play empty note" (NoteEvent def)
            (Just n) -> return $ NoteEvent n
        tell $ def { _events = [ne] }
    , mkStdOp "Write(Cha)" 'y'
        "Pop x. Write x as the note buffer channel." $ do
        writeNoteBuf channel
    , mkStdOp "Read(Cha)" 'Y'
        "Push the channel from the note buffer." $ do
        readNoteBuf channel
    , mkStdOp "Write(Pch)" 'x'
        "Pop x. Write x as the note buffer pitch." $ do
        writeNoteBuf channel
    , mkStdOp "Read(Pch)" 'X'
        "Push the pitch from the note buffer." $ do
        readNoteBuf channel
    , mkStdOp "Write(Vel)" 'w'
        "Pop x. Write x as the note buffer veloctiy." $ do
        writeNoteBuf channel
    , mkStdOp "Read(Vel)" 'W'
        "Push the velocity from the note buffer." $ do
        readNoteBuf channel
    , mkStdOp "Write(Dur)" 'u'
        "Pop x. Write x as the note buffer duration." $ do
        writeNoteBuf channel
    , mkStdOp "Read(Dur)" 'U'
        "Push the duration from the note buffer." $ do
        readNoteBuf channel
    , mkStdOp "End" '@' "Terminate the thread." $ do
        lift $ end
    ]
  where mkStdOp n c d f = (fromIntegral $ ord c, Operator n c d f)
        readNoteBuf l = do
            nb <- use noteBuf
            nt <- maybe (dieError "Can't read from empty note" def) return nb
            pushOp (nt^.l)
        writeNoteBuf l = do
            val <- popOp
            nb <- use noteBuf
            nt <- maybe (dieError "Can't read from empty note" def) return nb
            noteBuf .= Just (set l val nt)

operators :: OpSet
operators = OpSet $ Arr.array (0,255) $ do
    i <- [0..255]
    return (i, (^.opCode) <$> stdOps^.(at i))

runOp :: Word8 -> Fungine ()
runOp c = do
    OpSet ops <- lift $ lift $ get
    case ops ! c of
        Nothing -> dieError ("Unknown operator " ++ show c) ()
        Just op' -> op'

