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

module Tiler (Point, Tile(..), Tiler, newTiler, tile, untile) where

import System.Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Applicative
import Data.Maybe

type Point = (Integer, Integer)

data Tiler = Tiler Integer Integer [(Point -> Maybe Tile) -> Point -> Maybe Tile]

windowFun :: Tiler -> Point -> Maybe Tile
windowFun (Tiler _ _ tiles) = foldr (.) id tiles (const Nothing)

data Tile = Tile Char Point Point
  deriving (Read, Show, Eq, Ord)

instance Show Tiler where
    show win@(Tiler rows cols _) = showRows "" where
        tiles = windowFun win
        showRows = foldr (.) id [showRow r | r <- [0..rows - 1]] 
        showRow r = foldr (.) (showChar '\n') $ do
            c <- [0.. cols - 1]
            case tiles (r,c) of
                Nothing -> return $ showChar ' '
                Just (Tile ch _ _) -> return $ showChar ch

newTiler :: Integer -> Integer -> Tiler
newTiler rows cols = Tiler rows cols []

addTile :: Tile -> Tiler -> Tiler
addTile tile (Tiler rows cols tiles) = win where
    win = Tiler rows cols (tilefn:tiles)
    Tile _ (rmin, cmin) (rmax, cmax) = tile
    tilefn sub (r, c) =
        if r >= rmin && r <= rmax && c >= cmin && c <= cmax
            then return tile
            else sub (r, c)

tile :: RandomGen g => Integer -> Char -> Integer -> Integer -> Tiler -> g ->
    (Maybe (Tile, Tiler), g)
tile tries ch h w win@(Tiler rows cols _) = finder where
    finder = runState (runMaybeT (findSpot tries))
    tiles = windowFun win
    rrange = (0, rows - h)
    crange = (0, cols - w)
    tryCorns r c = msum $ fmap tiles [(r,c),(r+h-1,c),(r,c+w-1),(r+h-1,c+w-1)]
    trySpot r c = msum $ do
        r' <- [r..r+h-1]
        c' <- [c..c+w-1]
        return $ tiles (r', c')
    findSpot 0 = fail "no match"
    findSpot n = (<|> findSpot (n - 1)) $ do
        r <- state (randomR rrange)
        c <- state (randomR crange)
        case tryCorns r c <|> trySpot r c of
            Nothing ->
                let tile = Tile ch (r,c) (r+h-1, c+w-1)
                    win' = addTile tile win
                in return (tile, win')
            _ -> fail "occupied"

untile :: Tile -> Tiler -> Tiler
untile (Tile _ p _) (Tiler rows cols tiles) = Tiler rows cols tiles' where
    tiles' = filter (\f -> isNothing (f (const Nothing) p)) tiles

