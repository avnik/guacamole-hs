{-# LANGUAGE DeriveGeneric #-}
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
import qualified Generics.SOP                  as GS
import qualified GHC.Generics                  as GG

import           Network.Guacamole.Types.Enums (GuacamoleStatusEnum (..))

data PipeStatus = PipeStatus
    { psStream  :: !Int
    , psMessage :: !ByteString
    , psStatus  :: !GuacamoleStatusEnum
    } deriving (Eq, Show, GG.Generic)


data PipeRequest = PipeRequest
    { prStream :: !Int
    , prMime   :: !ByteString
    } deriving (Eq, Show, GG.Generic)

data PipeRequestNamed = PipeRequestNamed
    { prnStream :: !Int
    , prnName   :: !ByteString
    , prnMime   :: !ByteString
    } deriving (Eq, Show, GG.Generic)

data PipeRequestIndexed = PipeRequestIndexed
    { priStream :: !Int
    , priIndex  :: !Int
    , priMime   :: !ByteString
    } deriving (Eq, Show, GG.Generic)


data PipeBlob = PipeBlob
    { pbStream :: !Int
    , pbBlob   :: !ByteString
    } deriving (Eq, Show, GG.Generic)

-- "body" command
data PipeObject = PipeObject
    { poIndex    :: !Int
    , poStream   :: !Int
    , poMimeType :: !ByteString
    , poName     :: !ByteString
    } deriving (Eq, Show, GG.Generic)

data PipeKind =
    KindAudio
  | KindVideo
  | KindFile !ByteString
  | KindFilesystem
  | KindImage
  | KindPipe !ByteString
  | KindClipboard
  deriving (Eq, Show, GG.Generic)


instance GS.Generic PipeRequest
instance GS.Generic PipeRequestNamed
instance GS.Generic PipeRequestIndexed
instance GS.Generic PipeStatus
instance GS.Generic PipeBlob
instance GS.Generic PipeObject
