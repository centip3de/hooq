{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lib
    ( someFunc
    ) where

import Lens.Micro

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--class Table t name ty | t -> name, t -> ty where
--    types :: name -> ty

data UserRecord = UserRecord
    { userId :: Integer
    }
    deriving (Eq, Show)

class HasId s a | s -> a where
    id :: Lens' s a
