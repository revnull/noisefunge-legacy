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

import Control.Applicative
import Options.Applicative
import Language.NoiseFunge.API

killSpec :: Parser Request
killSpec = StopProgram
    <$> optional (option auto (long "pid" <> short 'p'))
    <*> optional (strOption (long "name" <> short 'n'))
    <*> optional (strOption (long "reason" <> short 'r'))

desc :: InfoMod a
desc = fullDesc
    <> header "nfkill - noisefunge process killer"

main :: IO ()
main = do
    req <- execParser (info (helper <*> killSpec) desc)
    withAPIConnection $ \conn -> do
        sendBinary' req conn

