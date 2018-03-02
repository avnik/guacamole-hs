module Network.Guacamole.Server.Types (
                                        GuacamoleServerRequest(..)
                                      , TransformLayer(..)
    ) where

import           Universum

import           Data.ByteString (ByteString)

import           Network.Guacamole.Types (GuacamoleCapStyle, GuacamoleCompositeMode,
                                          GuacamoleJoinStyle, GuacamoleRGBA, GuacamoleSize,
                                          GuacamoleTransfer, GuacamoleXY, PipeStatus)


data GuacamoleServerRequest =
    Ack
      { gsrAck :: !PipeStatus
      }
  | Args
      { gsrArgs :: ![ByteString]
      }
  | Arc
      { gsrcLayer     :: !Int
      , gsrXY         :: !GuacamoleXY
      , gsrRadius     :: !Int
      , gsrStartAngle :: !Double
      , gsrEndAngle   :: !Double
      , gsrNegative   :: !Bool
      }
  | Blob
      { gsrBlob :: !ByteString
      }
  | Body
      { gsrObject :: !Int
      , gsrStream :: !Int
      , gsrMime   :: !ByteString
      , gsrName   :: !ByteString
      }
  | Cfill
      { gsrMode  :: !GuacamoleCompositeMode
      , gsrLayer :: !Int
      }
  | Clip
      { gsrLayer :: !Int
      }
  | Clipboard
      { gsrStream :: !Int
      , gsrMime   :: !ByteString
      }
  | Copy
      { gsrSourceLayer :: !Int
      , gsrSourceXY    :: !GuacamoleXY
      , gsrH           :: !Int
      , gsrW           :: !Int
      , gsrMode        :: !GuacamoleCompositeMode
      , gsrDestLayer   :: !Int
      , gsrDestXY      :: !GuacamoleXY
      }
  | Cstroke
      { gsrMode      :: !GuacamoleCompositeMode
      , gsrLayer     :: !Int
      , gsrCap       :: !GuacamoleCapStyle
      , gsrJoin      :: !GuacamoleJoinStyle
      , gsrThinkness :: !Int
      , gsrRGBA      :: !GuacamoleRGBA
      }
  | Curve
      { grsIndex    :: !Int
      , gsrCurveCp1 :: !GuacamoleXY
      , gsrCurveCp2 :: !GuacamoleXY
      , csrCurveCp3 :: !GuacamoleXY
      }
  | Disconnect
  | Dispose
      { gsrDisposeIndex     :: !Int
      }
  | Distort
      { gsrDistort          :: !TransformLayer
      }
  | End
      { gsrStreamIndex      :: !Int
      }
  | Log
      { gsrLogMessage       :: !ByteString
      }
  | File
      { gsrStreamIndex :: !Int
      , gsrMime        :: !ByteString
      , gsrFileName    :: !ByteString
      }
  | Filesystem
      { gsrFilesystemIndex :: !Int
      , gsrFilesystemName  :: !ByteString
      }
  | Identity
      { gsrIdentityIndex    :: !Int
      }
  | Lfill
      { gsrLayerIndex  :: !Int
      , gsrSourceIndex :: !Int
      }
  | Line
      { gsrLine             :: !GuacamoleXY
      }
  | Lstroke
      { gsrMode      :: !GuacamoleCompositeMode
      , gsrIndex     :: !Int
      , gsrCap       :: !GuacamoleCapStyle
      , gsrJoin      :: !GuacamoleJoinStyle
      , gsrThinkness :: !Int
      , gsrSource    :: !Int
      }
  | Mouse
      { gsrMouse            :: !GuacamoleXY
      }
  | Move
      { gsrIndex  :: !Int
      , gsrParent :: !Int
      , gsrTo     :: !Int
      , gsrZ      :: !Int
      }
  | Name
      { gsrName             :: !ByteString
      }
  | Nest
      { gsrNest             :: !ByteString
      }
  | Nop
  | Pipe
      { gsrIndex :: !Int
      , gsrMime  :: !ByteString
      , gsrName  :: !ByteString
      }

  | Img
      { gsrStream :: !Int
      , gsrMode   :: !GuacamoleCompositeMode
      , gsrLayer  :: !Int
      , gsrMime   :: !ByteString
      , gsrXY     :: !GuacamoleXY
      }
  | Pop
      { gsrIndex            :: !Int
      }
  | Push
      { gsrIndex            :: !Int
      }
  | Ready
      { gsrSessionId        :: !Int
      }
  | Reset
      { gsrLayer            :: !Int
      }
  | Rect
      { gsrLayer :: !Int
      , gsrXY    :: !GuacamoleXY
      , gsrSize  :: !GuacamoleSize
      }
  | Select
      { gsrProtocol         :: !ByteString
      }
  | Set
      { gsrLayer :: !Int
      , gsrKey   :: !ByteString
      , gsrValue :: !ByteString
      }
  | Shade
      { gsrLayer :: !Int
      , gsrA     :: !Int
      }
  | Size
      { gsrSize             :: !GuacamoleSize
      }
  | Start
      { gsrLayer :: !Int
      , gsrXY    :: !GuacamoleXY
      }
  | Sync
      { gsrTimestamp        :: !Int
      }
  | Transfer
      { gsrSrcLayer  :: !Int
      , gsrSrc       :: !GuacamoleXY
      , gsrSize      :: !GuacamoleSize
      , gsrTransfer  :: !GuacamoleTransfer
      , gsrDestLayer :: !Int
      , gsrDest      :: !GuacamoleXY
      }
  | Transform
      { gsrTransform        :: !TransformLayer
      }
  | Undefine
      { gsrObject           :: !Int
      }
  | Video
      { gsrStream :: !Int
      , gsrLayer  :: !Int
      , gsrMime   :: !ByteString
      }
  | Unknown
      { gsrUnknown     :: !ByteString
      , gsrUnknownArgs :: ![ByteString]
      }

data TransformLayer = TransformLayer
    { tlIndex :: !Int
    , tlA     :: !Double
    , tlB     :: !Double
    , tlC     :: !Double
    , tlD     :: !Double
    , tlE     :: !Double
    , tlF     :: !Double
    }
