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

module Language.NoiseFunge.API (Tempo(Tempo), bpm, subbeats,
                                Beat(Beat), beat, subbeat,
                                (##),
                                Subscription(..),
                                Request(..),
                                Response(..),
                                withAPIConnection,
                                Conn,
                                sendBinary',
                                addProgram,
                                streamEvents,
                                PID, BefungeStats,
                                VMStats(..),
                                ExecStats(..),
                                vmPID, vmExec, vmMisc,
                                ProgArray,
                                ProcessStats,
                                psTicks, psStackSize,
                                psQuote,
                                Dir(..),
                                PC(..),
                                pos, dir, Pos,
                                Delta(..),
                                oldpc, newpc, change, events,
                                Event(..),
                                ) where

import Language.NoiseFunge.Beat
import Language.NoiseFunge.Befunge
import Language.NoiseFunge.Befunge.Process
import Language.NoiseFunge.Befunge.VM
import Language.NoiseFunge.Server.Comm

