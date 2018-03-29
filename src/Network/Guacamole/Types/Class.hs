{-# LANGUAGE DefaultSignatures #-}

module Network.Guacamole.Types.Class (
                                        GuacamoleEnumEncoding(..)
                                     ) where

import           Universum

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



