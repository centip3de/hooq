module Hooq.MySQL.Message where

import Data.Int
import Data.Word

import qualified Data.ByteString.Char8 as C

data MySQLResponsePacket = ResponseOK OKPacket | ResponseError ERRPacket
    
data OKPacket = OKPacket
    Word8 -- header
    Word64 -- affected_rows, max size = 8 bytes
    Word64 -- last_inser_id, max size = 8 bytes
    Word16 -- status flags
    Word16 -- warnings
    String -- info
    String -- session_state_changes
    String -- EOF info
    deriving (Show)

data ERRPacket = ERRPacket
    Word8 -- header
    Word16 -- error code
    String -- sql state marker
    String -- sql state
    String -- error message
    deriving (Show)
