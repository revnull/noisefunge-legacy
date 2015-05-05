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
import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Trans

import Data.Array.Unboxed
import Data.Char
import Data.IORef
import qualified Data.Map as M
import Data.Monoid
import Data.Typeable
import Data.Word

import Options.Applicative hiding (str)

import System.Random

import UI.NCurses as Curses

import Tiler
import Language.NoiseFunge.API

newtype Splitter = Splitter { unSplitter :: (Word32, Word32) }

instance Read Splitter where
    readsPrec p = readParen (p > 7)
        (\r -> [(Splitter (x,y),u) | (x,s)   <- readsPrec 8 r,
                ("/",t) <- lex s,
                (y,u)   <- readsPrec 8 t ])

data ViewerOpts = ViewerOpts {
    _distributor :: Maybe Splitter
  } 

$(makeLenses ''ViewerOpts)

optsSpec :: Parser ViewerOpts
optsSpec = ViewerOpts
    <$> optional (option auto (long "distribution" <> short 'd'))

desc :: InfoMod a
desc = fullDesc
    <> header "nfviewer - noisefunge viewer"

data ViewerState = ViewerState {
    _procs :: M.Map PID (Maybe (Tile, Window, Delta -> Curses ())),
    _tiler :: Tiler,
    _gen :: StdGen
  }

$(makeLenses ''ViewerState)

data TextBuf = TB ColorID Integer Window (IORef String)

safeChars :: String -> String
safeChars str = do
    c <- str
    if isPrint c
        then return c
        else return ' '

newTextBuf :: ColorID -> Integer -> Int -> Window -> IO TextBuf
newTextBuf col row size win =
    TB col row win <$> newIORef (take size (repeat ' '))

putTextBuf :: TextBuf -> String -> Curses ()
putTextBuf (TB c r w s) st = do
    str <- liftIO $ readIORef s
    let st' = concatMap escape st
        str' = drop (length st') $ str ++ st'
    liftIO $ writeIORef s str'
    updateWindow w $ do
        moveCursor r 0
        setColor c
        drawString . safeChars $ str'
  where escape '\n' = "\\n"
        escape ch = [ch]

highlight :: ProgArray -> Pos -> ColorID -> Update ()
highlight arr (r,c) col = do
    moveCursor (fromIntegral r + 1) (fromIntegral c)
    setColor col
    drawString . safeChars $ [chr . fromIntegral $ (arr ! (r, c))]

drawBoard :: ProgArray -> Update ()
drawBoard arr = do
    let (_, (rm, cm)) = bounds arr
    moveCursor 1 0
    setColor defaultColorID
    forM_ [0..rm] $ \row -> do
        moveCursor (fromIntegral row + 1) 0
        let str = [chr . fromIntegral $ (arr ! (row, col)) | col <- [0..cm]]
        drawString . safeChars $ str

newProgView :: ViewerOpts -> ColorID -> ColorID -> ColorID -> TextBuf ->
    PID -> ProgArray ->
    S.StateT ViewerState Curses (Maybe (Tile, Window, Delta -> Curses ()))
newProgView opts hdr exec out tbe (pnum, pnam) parr = res where
    (_, (rm, cm)) = bounds parr
    rm' = fromIntegral rm
    cm' = fromIntegral cm
    filtn = fst . unSplitter <$> opts^.distributor
    filtd = snd . unSplitter <$> opts^.distributor
    filtf d n x = n == (x `mod` d)
    filt x = filtf <$> filtd <*> filtn <*> pure x
    res = do
        tilr <- use tiler
        tile' <- gen %%= tile 5 'x' (rm' + 3) (cm' + 2) tilr
        let valid = maybe True id (filt pnum)
        case (valid, tile') of
            (False, _) -> return Nothing
            (_, Nothing) -> return Nothing
            (_, Just (til@(Tile _ tl _), tilr')) -> do
                tiler .= tilr'
                (w, fn) <- lift (viewer tl)
                return $ Just (til, w, fn)
    viewer (r, c) = do
        w <- newWindow (rm' + 3) (cm' + 2) r c
        updateWindow w $ do
            moveCursor 0 0
            setColor hdr
            let hname = take (fromIntegral cm' + 1) (name ++ repeat ' ')
                name = show pnum ++ (':':pnam)
            drawString hname
        tb <- liftIO $ newTextBuf out (rm' + 2) (fromIntegral cm' + 1) w
        putTextBuf tb " "
        aref <- liftIO $ newIORef parr
        updateWindow w $ drawBoard parr
        return . (w,) $ \delt -> do
            arr <- liftIO $ readIORef aref
            arr' <- do
                forM_ (delt^.events) $ \ev -> case ev of
                    StringEvent str -> putTextBuf tb str
                    ErrorEvent e -> putTextBuf tbe e
                    _ -> return ()
                arr' <- case delt^.change of
                    Nothing -> return arr
                    Just arr' -> updateWindow w $ do
                        drawBoard arr'
                        return arr'
                updateWindow w $ do
                    case delt^.oldpc of
                        Nothing -> return ()
                        Just pc -> do
                            highlight arr' (pc^.pos) defaultColorID
                    case delt^.newpc of
                        Nothing -> return ()
                        Just pc -> do
                            highlight arr' (pc^.pos) exec

                return arr'

            liftIO $ writeIORef aref arr'

clearWindow :: Tile -> Window -> Curses ()
clearWindow (Tile _ (r,c) (rx, cx)) w = updateWindow w $ do
    setColor defaultColorID
    let rm = rx - r
        st = [' ' | _ <- [c..cx - 1]]
    forM_ [0..rm] $ \row -> do
        moveCursor row 0
        drawString st

data ViewerException = Redraw | Quit
    deriving (Show, Typeable)

instance Exception ViewerException

main :: IO ()
main = withAPIConnection $ \conn -> do
    let loop = mainCurses conn `catch` handler
        handler Redraw = loop
        handler Quit = return ()
    loop

getAllEvents :: Window -> Curses [Curses.Event]
getAllEvents w = allEvs where
    allEvs = do
        ev <- getEvent w (Just 0)
        case ev of
            Nothing -> return []
            Just ev' -> (ev':) <$> allEvs

redraw :: S.StateT ViewerState Curses b
redraw = do
    zoom (procs.traverse) $ do
        pr <- S.get
        lift $ case pr of
            Nothing -> return ()
            Just (_, w, _) -> closeWindow w
    liftIO $ throwIO Redraw

mainCurses :: Conn -> IO ()
mainCurses conn = do
    opts <- execParser (info (helper <*> optsSpec) desc)
    runCurses $ do
        setEcho False
        _ <- setCursorMode CursorInvisible
        w <- defaultWindow
        (r, c) <- screenSize

        updateWindow w $ do
            setColor defaultColorID
            let blank = take (fromIntegral $ c - 1) $ repeat ' '
            forM_ [0..r-1] $ \y -> do
                moveCursor y 0
                drawString blank
        
        exec <- newColorID ColorBlack ColorWhite 1
        out <- newColorID ColorWhite ColorBlue 2
        err <- newColorID ColorWhite ColorRed 3
        hdr <- newColorID ColorBlack ColorCyan 4
        tbe <- liftIO $ newTextBuf err (r - 2) (fromIntegral c) w
        putTextBuf tbe " "

        let newView = newProgView opts hdr exec out tbe

        render

        vs <- ViewerState mempty (newTiler (r - 2) c) <$> liftIO newStdGen

        void $ flip S.runStateT vs $ streamEvents [Deltas] conn $ \ev -> do
            cevs <- lift $ getAllEvents w
            forM_ cevs $ \cev -> do
                case cev of
                    EventResized -> redraw
                    EventCharacter 'r' -> redraw
                    EventCharacter 'R' -> redraw
                    EventCharacter 'q' -> liftIO $ throwIO Quit
                    EventCharacter 'Q' -> liftIO $ throwIO Quit
                    _ -> return ()
            case ev of
                NextBeat bt -> do
                    lift $ updateWindow w $ do
                        moveCursor (r-1) 0
                        setColor defaultColorID
                        drawString $ show bt
                Catchup _ pid arr delt -> do
                    prc <- use (procs.(at pid))
                    case prc of
                        Nothing -> do
                            pv <- newView pid arr
                            case pv of
                                Nothing -> do
                                    procs.(at pid) .= Just Nothing
                                pv'@(Just (_, _, fn)) -> do
                                    procs.(at pid) .= Just pv'
                                    lift $ fn delt
                        Just Nothing -> return ()
                        Just (Just (_, _, fn)) ->
                            lift $ fn delt
                Change _ pid delt -> do
                    prc <- use (procs.(at pid))
                    case (prc, delt^.change) of
                        (Nothing, Just ch) -> do
                            pv <- newView pid ch
                            case pv of
                                Nothing -> procs.(at pid) .= Just Nothing
                                pv'@(Just (_, _, fn)) -> do
                                    procs.(at pid) .= Just pv'
                                    lift $ fn delt
                        (Nothing, _) -> do
                            return ()
                        (Just Nothing, _) -> return ()
                        (Just (Just (_, _, fn)), _) ->
                            lift $ fn delt
                    return ()
                Dead _ pid m -> do
                    prc <- use (procs.(at pid))
                    case prc of
                        Just (Just (t, w', _)) -> do
                            lift $ clearWindow t w'
                            procs.(at pid) .= Nothing
                            tiler %= untile t
                            lift $ closeWindow w'
                        Just Nothing -> do
                            procs.(at pid) .= Nothing
                        _ -> return ()
                    case m of
                        Nothing -> return ()
                        Just m' -> do
                            lift $ putTextBuf tbe (show pid)
                            lift $ putTextBuf tbe ":"
                            lift $ putTextBuf tbe m'
                            lift $ putTextBuf tbe "|"
                    return ()
                Reset -> redraw
                _ -> return ()
            lift $ render

