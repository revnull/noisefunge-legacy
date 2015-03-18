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

{-# LANGUAGE FlexibleContexts, TupleSections #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Identity

import Data.ConfigFile as CF
import qualified Data.Map as M
import Network.Socket

import System.Environment
import System.IO

import Language.NoiseFunge

getConfFile :: ErrorT (CPErrorData, [Char]) IO String
getConfFile = do
    args <- liftIO $ getArgs
    env <- lookup "NOISEFUNGE_CONFIG" <$> liftIO getEnvironment
    case (args, env) of
        ((conf:_), _) -> return conf
        ([], Just env') -> return env'
        _ -> fail "No config file"

parseConf :: (MonadError CPError m, Functor m, Applicative m, MonadIO m,
    Alternative m) => ConfigParser -> m ServerConfig
parseConf conf = sc where
    sc = ServerConfig <$>
        (concat <$> parseHosts) <*>
        (M.fromList <$> parsePorts) <*>
        parseTempo <*>
        parseOpParams <*>
        get conf "DEFAULT" "packet_size"
 
    servers = [srv | srv <- sections conf, "server" == take 6 srv]
    parseHosts = forM servers $ \sect -> do
        h <- get conf sect "host"
        p <- get conf sect "port"
        let hints = Just defaultHints {addrSocketType = Datagram }
        ais <- liftIO $ getAddrInfo hints (Just h) (Just p)
        forM ais $ \ai -> do
            return (addrFamily ai, addrAddress ai)
    ports = [prt | prt <- sections conf, "port" == take 4 prt]
    parsePorts = forM ports $ \sect -> do
        conn <- Just <$> get conf sect "connection" <|> return Nothing
        chan <- get conf sect "starting_channel" <|> return 0
        return (sect, ALSAPort conn chan)
    parseTempo = Tempo
        <$> get conf "DEFAULT" "beats"
        <*> get conf "DEFAULT" "subbeats"
    parseOpParams = OperatorParams
        <$> not <$> get conf "DEFAULT" "ignoreerror"
        <*> get conf "DEFAULT" "wrap"
        <*> get conf "DEFAULT" "debug"

defaultConf :: ConfigParser
Right defaultConf = defConf where
    defConf = runIdentity $ runErrorT $
        return emptyCP >>=
        setDef "DEFAULT" "packet_size" "4096" >>=
        setDef "DEFAULT" "ignoreerror" "False" >>=
        setDef "DEFAULT" "wrap" "False" >>=
        setDef "DEFAULT" "debug" "False" >>=
        addSec "port0" >>=
        setDef "port0" "starting_channel" "0"
    setDef s k v cp = set cp s k v
    addSec n cp = add_section cp n

main :: IO ()
main = do
    conf <- runErrorT $ do
        file <- getConfFile
        conf <- join $ liftIO $ readfile defaultConf file
        parseConf conf
    case conf of
        (Left err) -> hPutStrLn stderr (show err)
        (Right conf') -> runServer conf'

