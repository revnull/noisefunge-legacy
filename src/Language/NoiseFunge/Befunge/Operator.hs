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
                                             tellMem, operators,
                                             logError, logDebug
                                             ) where

import Control.Lens
import Control.Monad.RWS

import qualified Data.Array as Arr
import Data.Array.Unboxed
import Data.Char
import Data.Default
import Data.Word

import System.Random

import Language.NoiseFunge.Befunge.Process
import Language.NoiseFunge.Befunge.VM
import Language.NoiseFunge.Note

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

stdOp :: Word8 -> Maybe (Fungine ())
stdOp 0x20 = Just $ do -- ' '
    return ()

stdOp 0x3c = Just $ do -- <
    pc.dir .= L

stdOp 0x3e = Just $ do -- >
    pc.dir .= R

stdOp 0x76 = Just $ do -- v
    pc.dir .= D

stdOp 0x5e = Just $ do -- ^
    pc.dir .= U

stdOp 0x30 = Just $ do -- 0
    pushOp 0

stdOp 0x31 = Just $ do -- 1
    pushOp 1

stdOp 0x32 = Just $ do -- 2
    pushOp 2

stdOp 0x33 = Just $ do -- 3
    pushOp 3

stdOp 0x34 = Just $ do -- 4
    pushOp 4

stdOp 0x35 = Just $ do -- 5
    pushOp 5

stdOp 0x36 = Just $ do -- 6
    pushOp 6

stdOp 0x37 = Just $ do -- 7
    pushOp 7

stdOp 0x38 = Just $ do -- 8
    pushOp 8

stdOp 0x39 = Just $ do -- 9
    pushOp 9

stdOp 0x41 = Just $ do -- A
    pushOp 10

stdOp 0x42 = Just $ do -- B
    pushOp 11

stdOp 0x43 = Just $ do -- C
    pushOp 12

stdOp 0x44 = Just $ do -- D
    pushOp 13

stdOp 0x45 = Just $ do -- E
    pushOp 14

stdOp 0x46 = Just $ do -- F
    pushOp 15

stdOp 0x4b = Just $ do -- K
    new <- lift $ fork
    if new
        then do
            ticks .= 0
            pushOp 1
            tellMem
        else pushOp 0

stdOp 0x67 = Just $ do -- g
    y <- popOp
    x <- popOp
    arr <- use mem
    pushOp (arr ! (y,x))

stdOp 0x70 = Just $ do -- p
    y <- popOp
    x <- popOp
    v <- popOp
    arr <- use mem
    let arr' = arr // [((y,x),v)]
    mem .= arr'
    tellMem

stdOp 0x5c = Just $ do -- \
    a <- popOp
    b <- popOp
    pushOp a
    pushOp b

stdOp 0x24 = Just $ do -- $
    void popOp

stdOp 0x23 = Just $ do -- #
    jump .= True

stdOp 0x22 = Just $ do -- "
    quote .= True

stdOp 0x5b = Just $ do -- [
    fnStack .= Just []

stdOp 0x3a = Just $ do -- :
    a <- popOp
    pushOp a
    pushOp a

stdOp 0x7c = Just $ do -- |
    a <- popOp
    if a == 0
        then pc.dir .= D
        else pc.dir .= U

stdOp 0x5f = Just $ do -- _
    a <- popOp
    if a == 0
        then pc.dir .= R
        else pc.dir .= L

stdOp 0x3f = Just $ do -- ?
    r <- lift . rand $ randomR (0, 3)
    let d = case (r :: Int) of
            0 -> L
            1 -> R
            2 -> U
            3 -> D
            _ -> error "Random failure in ?"
    pc.dir .= d

stdOp 0x72 = Just $ do -- r
    r <- lift . rand $ random
    pushOp r

stdOp 0x52 = Just $ do -- R
    a <- popOp
    b <- popOp
    r <- lift . rand $ randomR (b, a)
    pushOp r

stdOp 0x21 = Just $ do -- !
    a <- popOp
    if a == 0
        then pushOp 1
        else pushOp 0

stdOp 0x60 = Just $ do -- `
    a <- popOp
    b <- popOp
    if b > a
        then pushOp 1
        else pushOp 0

stdOp 0x2b = Just $ do -- +
    a <- popOp
    b <- popOp
    pushOp (a + b)

stdOp 0x2d = Just $ do -- -
    a <- popOp
    b <- popOp
    pushOp (b - a)

stdOp 0x2a = Just $ do -- *
    a <- popOp
    b <- popOp
    pushOp (a * b)

stdOp 0x2f = Just $ do -- /
    a <- popOp
    b <- popOp
    if a /= 0
        then pushOp (b `div` a)
        else do
            dieError "Divide by zero" 255 >>= pushOp

stdOp 0x25 = Just $ do -- %
    a <- popOp
    b <- popOp
    if a /= 0
        then pushOp (b `mod` a)
        else do
            dieError "Modulo by zero" 0 >>= pushOp

stdOp 0x2e = Just $ do -- .
    a <- popOp
    outbuf <- use progOut
    lift $ writeBuf outbuf a

stdOp 0x3b = Just $ do -- ;
    a <- popOp
    outbuf <- use progOut
    lift $ bcastBuf outbuf a

stdOp 0x2c = Just $ do -- ,
    a <- (chr . fromIntegral) `fmap` popOp
    tell $ def { _events = [StringEvent [a]] }

stdOp 0x26 = Just $ do -- &
    a <- popOp
    tell $ def { _events = [StringEvent $ show a] }

stdOp 0x7e = Just $ do -- ~
    inbuf <- use progIn
    ch <- lift $ readBuf inbuf
    pushOp ch

stdOp 0x4d = Just $ do -- M (major chord)
    nb <- use noteBuf
    n <- case nb of
        Nothing -> dieError "Can't play empty note" def
        (Just n) -> return n
    let nes = do
            ch <- [0, 4, 7]
            return $ NoteEvent (pitch %~ (ch +) $ n)
    tell $ def { _events = nes }

stdOp 0x6d = Just $ do -- m (minor chord)
    nb <- use noteBuf
    n <- case nb of
        Nothing -> dieError "Can't play empty note" def
        (Just n) -> return n
    let nes = do
            ch <- [0, 3, 7]
            return $ NoteEvent (pitch %~ (ch +) $ n)
    tell $ def { _events = nes }

stdOp 0x4e = Just $ do -- N (major 7th chord)
    nb <- use noteBuf
    n <- case nb of
        Nothing -> dieError "Can't play empty note" def
        (Just n) -> return n
    let nes = do
            ch <- [0, 4, 7, 11]
            return $ NoteEvent (pitch %~ (ch +) $ n)
    tell $ def { _events = nes }

stdOp 0x6e = Just $ do -- n (minor 7th chord)
    nb <- use noteBuf
    n <- case nb of
        Nothing -> dieError "Can't play empty note" def
        (Just n) -> return n
    let nes = do
            ch <- [0, 3, 7, 11]
            return $ NoteEvent (pitch %~ (ch +) $ n)
    tell $ def { _events = nes }

stdOp 0x7a = Just $ do -- z (write buffer)
    dur <- popOp
    vel <- popOp
    pit <- popOp
    cha <- popOp
    noteBuf .= Just (Note cha pit vel dur)

stdOp 0x5a = Just $ do -- Z (play buffer)
    nb <- use noteBuf
    ne <- case nb of
        Nothing -> dieError "Can't play empty note" (NoteEvent def)
        (Just n) -> return $ NoteEvent n
    tell $ def { _events = [ne] }

stdOp 0x79 = Just $ do -- y (write channel)
    writeNoteBuf channel

stdOp 0x59 = Just $ do -- Y (read channel)
    readNoteBuf channel

stdOp 0x78 = Just $ do -- x (write pitch)
    writeNoteBuf pitch

stdOp 0x58 = Just $ do -- X (read pitch)
    readNoteBuf pitch

stdOp 0x77 = Just $ do -- w (write velocity)
    writeNoteBuf velocity

stdOp 0x57 = Just $ do -- W (read velocity)
    readNoteBuf velocity

stdOp 0x75 = Just $ do -- u (write duration)
    writeNoteBuf duration

stdOp 0x55 = Just $ do -- U (read duration)
    readNoteBuf duration

stdOp 0x40 = Just $ do -- @
    lift $ end

stdOp _ = Nothing -- Noop

operators :: OpSet
operators = OpSet $ Arr.array (0,255) $ do
    i <- [0..255]
    return (i, stdOp i)

runOp :: Word8 -> Fungine ()
runOp c = do
    OpSet ops <- lift $ lift $ get
    case ops ! c of
        Nothing -> dieError ("Unknown operator " ++ show c) ()
        Just op' -> op'

readNoteBuf :: Simple Lens Note Word8 -> Fungine ()
readNoteBuf l = do
    nb <- use noteBuf
    nt <- maybe (dieError "Can't read from empty note" def) return nb
    pushOp (nt^.l)

writeNoteBuf :: Simple Lens Note Word8 -> Fungine ()
writeNoteBuf l = do
    val <- popOp
    nb <- use noteBuf
    nt <- maybe (dieError "Can't read from empty note" def) return nb
    noteBuf .= Just (set l val nt)

