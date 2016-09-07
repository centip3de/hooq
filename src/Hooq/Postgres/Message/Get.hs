{-# OverloadedStrings #-}
module Hooq.Postgres.Message.Get (getMessage) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Char
import Data.Word

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C

import Hooq.Postgres.Message

getMessage :: Get Message
getMessage = commandComplete
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

-- CommandComplete
commandComplete :: Get Message
commandComplete = do
    getType 'C'
    len <- getWord32be
    tag <- getCString'
    return $ CommandComplete tag

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
