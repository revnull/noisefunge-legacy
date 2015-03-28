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

{-# LANGUAGE TemplateHaskell, TupleSections, DeriveDataTypeable #-}

import Prelude hiding (catch)

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.Trans
import Control.Monad
import qualified Control.Monad.State as S

import qualified Data.Map as M
import Data.Monoid
import Data.Typeable

import Text.Printf

import UI.NCurses as Curses

import Language.NoiseFunge.API

data ViewerState = ViewerState {
    _procs :: M.Map PID BefungeStats
  }

$(makeLenses ''ViewerState)

main :: IO ()
main = withAPIConnection $ \conn -> do
    let loop = mainCurses conn `catch` handler
        handler Redraw = loop
        handler Quit = return ()
    loop

formatLine :: PrintfType t => VMStats ProcessStats -> t
formatLine stats = formatted where
    formatted = printf fmt pnum pnam' t ex ss q
    fmt = "%6d %-14s %5s %-6s %5s %c"
    (pnum, pnam) = stats^.vmPID
    t = ticks (stats^.vmMisc.psTicks)
    ex = execs (stats^.vmExec)
    ss = ticks (stats^.vmMisc.psStackSize)
    q = if (stats^.vmMisc.psQuote) then 'Q' else ' '
    pnam' = take 10 pnam

data ViewerException = Redraw | Quit
    deriving (Show, Typeable)

instance Exception ViewerException

getAllEvents :: Window -> Curses [Curses.Event]
getAllEvents w = allEvs where
    allEvs = do
        ev <- getEvent w (Just 0)
        case ev of
            Nothing -> return []
            Just ev' -> (ev':) <$> allEvs

mainCurses :: Conn -> IO ()
mainCurses conn = 
    runCurses $ do
        setEcho False
        void $ setCursorMode CursorInvisible
        w <- defaultWindow
        (r, c) <- screenSize
        glob <- newColorID ColorBlack ColorWhite 1
        updateWindow w $ do
            moveCursor 1 0
            setColor glob
            drawString "   PID Process        Ticks Status Stack Q"
        let vs = ViewerState mempty
            blankLine = take (fromIntegral $ c - 1) $ repeat ' '
        updateWindow w $ forM_ [2..r-1] $ \i -> do
            moveCursor i 0
            drawString blankLine
        void $ flip S.runStateT vs $ streamEvents [Stats] conn $ \ev -> do
            cevs <- lift $ getAllEvents w
            forM_ cevs $ \cev -> do
                case cev of
                    EventResized -> throw Redraw
                    EventCharacter 'q' -> liftIO $ throwIO Quit
                    EventCharacter 'Q' -> liftIO $ throwIO Quit
                    _ -> return ()
            case ev of
                TickStats bt run rbl wbl ded -> do
                    lift $ updateWindow w $ do
                        moveCursor 0 0
                        setColor defaultColorID
                        let line = printf fmt (bt^.beat) (bt^.subbeat)
                                run rbl wbl ded
                            fmt = "%9d|%-4d X:%-4d R:%-4d W:%-4d H:%-4d"
                        drawString line
                    ps <- use procs
                    lift $ updateWindow w $ do
                        setColor defaultColorID
                        forM_ [2..r-1] $ \i -> do
                            moveCursor i 0
                            drawString blankLine
                        forM_ (zip [2..r-1] (M.elems ps)) $ \(i, p) -> do
                            moveCursor i 0
                            drawString (formatLine p)
                    let ps' = M.filter (livingExec . (^.vmExec)) ps
                        livingExec (EHalted _) = False
                        livingExec _ = True
                    procs .= ps'
                ProcessStats _ st -> do
                    let pid = st^.vmPID
                    procs.(at pid) .= Just st
                Reset -> throw Redraw
                _ -> return ()
            lift $ render

ticks :: (Show i, Integral i) => i -> String
ticks n
    | n < 1000 = show n
    | n < 1000000 = show (n `div` 1000) ++ "K"
    | n < 1000000000 = show (n `div` 1000000) ++ "M"
    | otherwise = show (n `div` 1000000000) ++ "G"


execs :: ExecStats -> [Char]
execs ERunning = "X"
execs (EHalted Nothing) = "H"
execs (EHalted (Just m)) = 'H' : take 5 m
execs (ERBlock buf) = 'R' : take 5 buf
execs (EWBlock buf) = 'W' : take 5 buf

