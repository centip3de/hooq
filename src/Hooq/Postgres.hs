{-# LANGUAGE OverloadedStrings #-}
module Hooq.Postgres where

import Hooq.Postgres.Message
import Hooq.Postgres.Message.Get
import Hooq.Postgres.Message.Put

import Data.Char

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

-- Raw message format
-- 1. First byte represents message type
-- 2. Next four bytes represents message length (including length itself)
-- 3. Contents of the message follow, structure varying on message type
recvRawMessage :: Socket -> IO C.ByteString
recvRawMessage sock = do
    ty <- recv sock 1
    len <- runGet getWord32be . B.fromStrict <$> recv sock 4
    msg <- recv sock $ (fromIntegral len) - 4
    return $ B.toStrict $ runPut $ do
        putByteString ty
        putWord32be len
        putByteString msg

type StartupData = M.Map (C.ByteString) (C.ByteString)

defaultData :: StartupData
defaultData = M.empty

startupMessage :: StartupData -> C.ByteString
startupMessage dat = B.toStrict $ runPut $ do
    let dict = startupParams dat
    putWord32be $ fromIntegral (C.length dict) + 8
    putWord32be 196608
    putByteString dict

appendNull :: C.ByteString -> C.ByteString
appendNull s = C.append s "\0"

startupParams :: StartupData -> C.ByteString
startupParams = appendNull . C.concat . map appendNull . concatMap go . M.assocs
    where go (k, v) = [k, v]

waitForReady :: Socket -> IO ()
waitForReady sock = do
    rawmsg <- recvRawMessage sock
    let msg = runGet getMessage (B.fromStrict rawmsg)
    print msg
    case msg of
        ReadyForQuery _ -> return ()
        _ -> waitForReady sock

getTableOidQuery :: C.ByteString
getTableOidQuery = appendNull $ C.concat
    [ " SELECT c.oid"
    , " FROM pg_catalog.pg_class c"
    , " LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace"
    , " WHERE c.relname = 'quotes'"
    , " AND pg_catalog.pg_table_is_visible(c.oid);"
    ]

getTableColumnsQuery :: C.ByteString
getTableColumnsQuery = appendNull $ C.concat
    [ " SELECT a.attname"
    , " pg_catalog.format_type(a.atttypid, a.atttypmod)"
    , " FROM pg_catalog.pg_attribute a"
    , " WHERE a.attrelid = '16782' AND a.attnum > 0 AND NOT a.attisdropped"
    , " ORDER BY a.attnum ASC;"
    ]

printMessages :: Socket -> IO ()
printMessages sock = do
    rawmsg <- recvRawMessage sock
    let msg = runGet getMessage (B.fromStrict rawmsg)
    print msg
    printMessages sock

getTable :: Socket -> IO ()
getTable sock = do
    let msg = B.toStrict $ runPut $ putMessage (Query getTableOidQuery)
    sendAll sock msg
    printMessages sock

run :: IO ()
run = withSocketsDo $ do
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "5432")
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock $ addrAddress addr
    let msg = startupMessage $ M.fromList [("user", "ujboldoppi4v4ge7kyfc"), ("database", "qdb")]
    sendAll sock msg
    waitForReady sock
    getTable sock
    close sock
