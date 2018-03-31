{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
module Network.Guacamole.Types.Core (
                                      GuacamoleRGBA(..)
                                    , GuacamoleXY(..)
                                    , GuacamoleSize(..)
                                    , TransformLayer(..)
                                    ) where

import           Universum

import           Data.ByteString
import qualified Generics.SOP as SOP

import           Network.Guacamole.Types.Enums (GuacamoleStatusEnum (..))

data GuacamoleRGBA = GuacamoleRGBA
    { guacR :: !Int
    , guacG :: !Int
    , guacB :: !Int
    , guacA :: !Int
    } deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

data GuacamoleXY = GuacamoleXY
    { guacX :: !Int
    , guacY :: !Int
    } deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)


data GuacamoleSize = GuacamoleSize
    { guacH :: !Int
    , guacW :: !Int
    } deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)


data GuacamoleStatus = GuacamoleStatus
    { guacStatus  :: !GuacamoleStatusEnum
    , guacMessage :: !ByteString
    } deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

data TransformLayer = TransformLayer
    { tlIndex :: !Int
    , tlA     :: !Double
    , tlB     :: !Double
    , tlC     :: !Double
    , tlD     :: !Double
    , tlE     :: !Double
    , tlF     :: !Double
    } deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

