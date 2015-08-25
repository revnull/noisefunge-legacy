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

module Language.NoiseFunge.Note (Note(Note), channel, pitch,
                                 velocity, duration) where

import Control.Applicative
import Control.Lens

import Data.Default
import Data.Word
import Data.Binary

data Note = Note {
    _channel :: !Word8,
    _pitch :: !Word8,
    _velocity :: !Word8,
    _duration :: !Word8
  } deriving (Read, Show, Eq, Ord)

instance Default Note where
    def = Note 0 0 0 0

instance Binary Note where
    get = Note <$> get <*> get <*> get <*> get
    put (Note c p v d) = put c >> put p >> put v >> put d

$(makeLenses ''Note)

