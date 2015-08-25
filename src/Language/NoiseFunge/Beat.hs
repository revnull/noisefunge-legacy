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

module Language.NoiseFunge.Beat (Tempo(Tempo), bpm, subbeats,
                                 Beat(Beat), beat, subbeat,
                                 (##),
                                 Beats, beats) where

import Control.Applicative
import Control.Lens

import Data.Binary
import Data.Default

data Tempo = Tempo {
    _bpm :: !Word32,
    _subbeats :: !Word32
  } deriving (Read, Show, Eq, Ord)

$(makeLenses ''Tempo)

data Beat = Beat {
    _beat :: !Word32,
    _subbeat :: !Word32
  } deriving (Read, Eq, Ord)

$(makeLenses ''Beat)

instance Binary Beat where
    get = Beat <$> get <*> get
    put (Beat x y) = put x >> put y

instance Default Beat where
    def = Beat 0 0

instance Show Beat where
    show (Beat x y) = shows x . showChar '|' . shows y $ []

type Beats = [Beat]

(##) :: Tempo -> Beat -> Beat
(Tempo _ sb) ## (Beat b s) = nb where
    s' = s + 1
    b' = b + 1
    nb = if s' == sb
        then (Beat b' 0)
        else (Beat b s')

beats :: Tempo -> Beats
beats t = iterate (t ##) def

