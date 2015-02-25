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

{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Identity

import Data.ConfigFile as CF
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

parseConf :: (MonadError CPError m, Functor m, Applicative m, MonadIO m) =>
    ConfigParser -> m ServerConfig
parseConf conf = sc where
    sc = ServerConfig <$>
        (concat <$> parseHosts) <*>
        parseTempo <*>
        parseOpParams <*>
        get conf "DEFAULT" "packet_size"

    parseHosts = forM (sections conf) $ \sect -> do
        h <- get conf sect "host"
        p <- get conf sect "port"
        let hints = Just defaultHints {addrSocketType = Datagram }
        ais <- liftIO $ getAddrInfo hints (Just h) (Just p)
        forM ais $ \ai -> do
            return (addrFamily ai, addrAddress ai)
    parseTempo = Tempo
        <$> get conf "DEFAULT" "beats"
        <*> get conf "DEFAULT" "subbeats"
    parseOpParams = OperatorParams
        <$> get conf "DEFAULT" "ignoreerror"
        <*> get conf "DEFAULT" "wrap"
        <*> get conf "DEFAULT" "debug"

defaultConf :: ConfigParser
Right defaultConf = runIdentity $ runErrorT $ do
    cp <- set emptyCP "DEFAULT" "packet_size" "4096"
    cp' <- set cp "DEFAULT" "ignoreerror" "False"
    cp'' <- set cp' "DEFAULT" "wrap" "False"
    set cp'' "DEFAULT" "debug" "False"

main :: IO ()
main = do
    conf <- runErrorT $ do
        file <- getConfFile
        conf <- join $ liftIO $ readfile defaultConf file
        parseConf conf
    case conf of
        (Left err) -> hPutStrLn stderr (show err)
        (Right conf') -> runServer conf'

