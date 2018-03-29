{-# LANGUAGE DefaultSignatures #-}
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
import qualified Generics.SOP                  as GS
import qualified GHC.Generics                  as GG

import           Network.Guacamole.Types.Enums (GuacamoleStatusEnum (..))

data GuacamoleRGBA = GuacamoleRGBA
    { guacR :: !Int
    , guacG :: !Int
    , guacB :: !Int
    , guacA :: !Int
    } deriving (Eq, Ord, Show, GG.Generic)

data GuacamoleXY = GuacamoleXY
    { guacX :: !Int
    , guacY :: !Int
    } deriving (Eq, Ord, Show, GG.Generic)


data GuacamoleSize = GuacamoleSize
    { guacH :: !Int
    , guacW :: !Int
    } deriving (Eq, Ord, Show, GG.Generic)


data GuacamoleStatus = GuacamoleStatus
    { guacStatus  :: !GuacamoleStatusEnum
    , guacMessage :: !ByteString
    } deriving (Eq, Show, GG.Generic)

data TransformLayer = TransformLayer
    { tlIndex :: !Int
    , tlA     :: !Double
    , tlB     :: !Double
    , tlC     :: !Double
    , tlD     :: !Double
    , tlE     :: !Double
    , tlF     :: !Double
    } deriving (Eq, Show, GG.Generic)

instance GS.Generic GuacamoleXY
instance GS.Generic GuacamoleSize
instance GS.Generic GuacamoleRGBA
instance GS.Generic GuacamoleStatus
instance GS.Generic TransformLayer
