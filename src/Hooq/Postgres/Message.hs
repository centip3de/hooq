module Hooq.Postgres.Message where

import Data.Word
import qualified Data.ByteString.Char8 as C

data Message
    = BackendKeyData Word32 Word32
    | AuthenticationOk
    | ParameterStatus String String
    | ReadyForQuery TransactionStatus
    | Query C.ByteString
    | UnknownMessage Char Word32 C.ByteString
    deriving (Show)

data TransactionStatus
    = Idle
    | InTransaction
    | FailedTransaction
    deriving (Show)
