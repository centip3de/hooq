{-# OverloadedStrings #-}
module Hooq.Postgres.Message.Get (getMessage) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Char
import Data.Int
import Data.Word

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C

import Hooq.Postgres.Message

getMessage :: Get Message
getMessage = rowDescription
    <|> commandComplete
    <|> dataRow
    <|> backendKeyData
    <|> authenticationOk
    <|> parameterStatus
    <|> readyForQuery
    <|> unknownMessage

fromChar :: Char -> a -> Get a
fromChar c x = do
    c' <- chr <$> fromIntegral <$> getWord8
    guard $ c == c'
    return x

getType :: Char -> Get ()
getType c = do
    c' <- chr . fromIntegral <$> getWord8
    guard $ c == c'

getCString :: Get String
getCString = C.unpack <$> getCString'

getCString' :: Get C.ByteString
getCString' = do
    c <- getWord8
    if c == 0
        then return C.empty
        else C.cons (chr $ fromIntegral c) <$> getCString'

getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

-- RowDescription
fieldDescription :: Get FieldDescription
fieldDescription = do
    name <- getCString'
    tableObjectId <- getWord32be
    columnAttrNum <- getWord16be
    typeObjectId <- getWord32be
    typeSize <- getInt16be
    typeMod <- getWord32be
    formatCode <- getWord16be
    return $ FieldDescription name tableObjectId columnAttrNum typeObjectId typeSize typeMod formatCode

rowDescription :: Get Message
rowDescription = do
    getType 'T'
    len <- getWord32be
    count <- fromIntegral <$> getWord16be
    fields <- replicateM count fieldDescription
    return $ RowDescription fields

-- CommandComplete
commandComplete :: Get Message
commandComplete = do
    getType 'C'
    len <- getWord32be
    tag <- getCString'
    return $ CommandComplete tag

-- DataRow
dataRowColumn :: Get DataRowColumn
dataRowColumn = do
    len <- getInt32be
    if len == -1
        then return ColumnNull
        else ColumnData <$> getByteString (fromIntegral len)

dataRow :: Get Message
dataRow = do
    getType 'D'
    len <- getWord32be
    cols <- getWord16be
    DataRow <$> replicateM (fromIntegral cols) dataRowColumn

-- BackendKeyData
backendKeyData :: Get Message
backendKeyData = do
    getType 'K'
    len <- getWord32be
    guard $ len == 12
    processId <- getWord32be
    secretKey <- getWord32be
    return $ BackendKeyData processId secretKey

-- AuthenticationOk
authenticationOk :: Get Message
authenticationOk = do
    getType 'R'
    len <- getWord32be
    guard $ len == 8
    status <- getWord32be
    guard $ status == 0
    return AuthenticationOk

-- ParameterStatus
parameterStatus :: Get Message
parameterStatus = do
    getType 'S'
    len <- getWord32be
    param <- getCString
    val <- getCString
    return $ ParameterStatus param val

-- ReadyForQuery
transactionStatus :: Get TransactionStatus
transactionStatus = fromChar 'I' Idle
    <|> fromChar 'T' InTransaction
    <|> fromChar 'E' FailedTransaction

readyForQuery :: Get Message
readyForQuery = do
    getType 'Z'
    len <- getWord32be
    guard $ len == 5
    status <- transactionStatus
    return $ ReadyForQuery status

-- UnknownMessage
unknownMessage :: Get Message
unknownMessage = do
    ty <- chr . fromIntegral <$> getWord8
    len <- getWord32be
    msg <- getByteString $ (fromIntegral len) - 4
    return $ UnknownMessage ty len msg
