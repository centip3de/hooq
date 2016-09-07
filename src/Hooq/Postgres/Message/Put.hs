module Hooq.Postgres.Message.Put (putMessage) where

import Data.Binary.Put
import qualified Data.ByteString.Char8 as C
import Data.Char

import Hooq.Postgres.Message

putMessage :: Message -> Put
putMessage (Query q) = do
    putWord8 $ fromIntegral $ ord 'Q'
    putWord32be $ fromIntegral $ 4 + C.length q
    putByteString q
