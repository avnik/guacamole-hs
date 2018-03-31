{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Network.Guacamole.Types.Pipes (
                                       PipeStatus(..)
                                     , PipeRequest(..)
                                     , PipeRequestNamed(..)
                                     , PipeRequestIndexed(..)
                                     , PipeBlob(..)
                                     , PipeObject(..)
                                     )where

import           Universum

import           Data.ByteString
import qualified Generics.SOP as SOP

import           Network.Guacamole.Types.Enums (GuacamoleStatusEnum (..))

data PipeStatus = PipeStatus
    { psStream  :: !Int
    , psMessage :: !ByteString
    , psStatus  :: !GuacamoleStatusEnum
    } deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)


data PipeRequest = PipeRequest
    { prStream :: !Int
    , prMime   :: !ByteString
    } deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

data PipeRequestNamed = PipeRequestNamed
    { prnStream :: !Int
    , prnName   :: !ByteString
    , prnMime   :: !ByteString
    } deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

data PipeRequestIndexed = PipeRequestIndexed
    { priStream :: !Int
    , priIndex  :: !Int
    , priMime   :: !ByteString
    } deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)


data PipeBlob = PipeBlob
    { pbStream :: !Int
    , pbBlob   :: !ByteString
    } deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

-- "body" command
data PipeObject = PipeObject
    { poIndex    :: !Int
    , poStream   :: !Int
    , poMimeType :: !ByteString
    , poName     :: !ByteString
    } deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

data PipeKind =
    KindAudio
  | KindVideo
  | KindFile !ByteString
  | KindFilesystem
  | KindImage
  | KindPipe !ByteString
  | KindClipboard
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
