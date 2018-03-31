{-# LANGUAGE DeriveGeneric #-}

module Network.Guacamole.Types.Client (
                                       GuacamoleClientRequest(..)
    ) where

import           Universum

import           Data.ByteString (ByteString)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GG

import           Network.Guacamole.Types.Core (GuacamoleSize (..), GuacamoleXY (..))
import           Network.Guacamole.Types.Pipes (PipeBlob (..), PipeRequest (..),
                                                PipeRequestIndexed (..), PipeRequestNamed (..),
                                                PipeStatus (..))

data GuacamoleClientRequest =
    Sync
        { gcrTimestamp :: !Int
        }
  | Mouse
        { gcrMouse :: !GuacamoleXY
        , gcrMask  :: !Int
        }
  | Key
        { gcrKey   :: !Int
        , gcrPress :: !Int
        }
  | Clipboard
        { gcrClipboard :: !PipeRequest
        }
  | Disconnect
  | Size
        { gcrSize      :: !GuacamoleSize
        }
  | File
        { gcrFile      :: !PipeRequestNamed
        }
  | Pipe
        { gcrPipe :: !PipeRequestNamed
        }
  | Ack
        { gcrAck  :: !PipeStatus
        }
  | Blob
        { gcrBlob   :: !PipeBlob
        }
  | End
        { gcrStream :: !Int
        }
  | Get
        { gcrObject :: !Int
        , gcrName   :: !ByteString
        }
  | Put
        { gcrObject :: !Int
        , gcrStream :: !Int
        , gcrMime   :: !ByteString
        , gcrName   :: !ByteString
        }
  | Audio
        { gcrAudio  :: !PipeRequestIndexed
        }
  deriving (Eq, Show, GG.Generic)

instance SOP.Generic GuacamoleClientRequest
instance SOP.HasDatatypeInfo GuacamoleClientRequest
