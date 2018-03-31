{-# LANGUAGE DefaultSignatures #-}

module Network.Guacamole.Types.Class (
                                       GuacamoleEnumEncoding(..)
                                     ) where

import           Universum

import qualified Data.ByteString.Char8 as BS8

class GuacamoleEnumEncoding a where
    toGuacamoleEnum   :: Int -> a
    fromGuacamoleEnum :: a -> Int

    default toGuacamoleEnum :: Enum a => Int -> a
    toGuacamoleEnum = toEnum

    default fromGuacamoleEnum :: Enum a => a -> Int
    fromGuacamoleEnum = fromEnum

-- | FIXME: do we need it?
class GuacamoleValidate a where
    validGuacamole    :: a -> Bool

