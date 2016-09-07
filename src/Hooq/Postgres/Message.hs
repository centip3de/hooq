module Hooq.Postgres.Message where

import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as C

data Message
    = BackendKeyData Word32 Word32
    | AuthenticationOk
    | ParameterStatus String String
    | ReadyForQuery TransactionStatus
    | Query C.ByteString
    | CommandComplete C.ByteString
    | DataRow [DataRowColumn]
    | RowDescription [FieldDescription]
    | UnknownMessage Char Word32 C.ByteString
    deriving (Show)

data TransactionStatus
    = Idle
    | InTransaction
    | FailedTransaction
    deriving (Show)

data DataRowColumn
    = ColumnNull
    | ColumnData C.ByteString
    deriving (Show)

data FieldDescription = FieldDescription
    C.ByteString -- name
    Word32       -- table object id
    Word16       -- colunn attribute number
    Word32       -- type object id
    Int16        -- data type size
    Word32       -- type modifier
    Word16       -- format code (0/1 for text/binary)
    deriving (Show)
