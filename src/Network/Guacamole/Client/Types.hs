module Network.Guacamole.Client.Types (
                                       GuacamoleClientRequest
    ) where

import           Data.ByteString         (ByteString)
import           Network.Guacamole.Types (GuacamoleSize, GuacamoleXY,
                                          PipeRequest, PipeStatus)

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
        { gcrSize      :: !GuacamoleXY
        }
  | File
        { gcrFile      :: !PipeRequest
        }
  | Pipe
        { gcrPipe :: !PipeRequest
        }
  | Ack
        { gcrAck  :: !PipeStatus
        }
  | Blob
        { gcrStream :: !Int
        , gcrBlob   :: !ByteString
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
        , gcrMime   :: !Int
        , gcrName   :: !ByteString
        }
  | Audio
        { gcrAudio  :: !PipeRequest
        }
  deriving (Eq, Show)

