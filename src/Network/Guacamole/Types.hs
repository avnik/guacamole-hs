{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase        #-}
module Network.Guacamole.Types where

import           Universum

import           Data.ByteString

class GuacamoleEnumEncoding a where
    toGuacamoleEnum   :: Int -> a
    fromGuacamoleEnum :: a -> Int

    default toGuacamoleEnum :: Enum a => Int -> a
    toGuacamoleEnum = toEnum

    default fromGuacamoleEnum :: Enum a => a -> Int
    fromGuacamoleEnum = fromEnum

class GuacamoleValidate a where
    validGuacamole    :: a -> Bool

data GuacamoleRGBA = GuacamoleRGBA
    { guacR :: !Int
    , guacG :: !Int
    , guacB :: !Int
    , guacA :: !Int
    } deriving (Eq, Ord, Show)

data GuacamoleXY = GuacamoleXY
    { guacX :: !Int
    , guacY :: !Int
    } deriving (Eq, Ord, Show)


data GuacamoleSize = GuacamoleSize
    { guacH :: !Int
    , guacW :: !Int
    } deriving (Eq, Ord, Show)


data GuacamoleServerStatus =
    ServerError
  | ServerBusy
  | ServerUpstreamTimeout
  | UpstreamTimeout
  | UpstreamClosed
  | ResourceNotFound
  | ResourceClosed
  | ResourceConflict
  | UpstreamNotFound
  | UpstreamUnavailable
  | SessionConflict
  | SessionTimeout
  | SessionClosed
  deriving (Eq, Show, Enum)

data GuacamoleClientStatus =
    ClientBadRequest
  | ClientUnauthorized
  | ClientForbidden
  | ClientBadType
  | ClientTimeout
  | ClientOverrun
  | ClientTooMany
  | ClientUnknownCode !Int
  deriving (Eq, Show)

instance GuacamoleEnumEncoding GuacamoleClientStatus where
    toGuacamoleEnum = \case
        0x300 -> ClientBadRequest
        0x301 -> ClientUnauthorized
        0x303 -> ClientForbidden
        0x308 -> ClientTimeout
        0x30D -> ClientOverrun
        0x30F -> ClientBadType
        0x31D -> ClientTooMany
        x     -> ClientUnknownCode x

    fromGuacamoleEnum = \case
        ClientBadRequest -> 0x300
        ClientUnauthorized -> 0x301
        ClientForbidden -> 0x303
        ClientTimeout -> 0x308
        ClientOverrun -> 0x30D
        ClientBadType -> 0x30F
        ClientTooMany -> 0x31D
        ClientUnknownCode x -> x

data GuacamoleStatus = GuacamoleStatus
    { guacStatus  :: !GuacamoleStatusEnum
    , guacMessage :: !ByteString
    } deriving (Eq, Show)


data PipeStatus = PipeStatus
    { psStream  :: !Int
    , psStatus  :: !GuacamoleStatusEnum
    , psMessage :: !ByteString
    } deriving (Eq, Show)


data PipeRequest = PipeRequest
    { prStream :: !Int
    , prMime   :: !Int
    , prKind   :: !PipeKind
    } deriving (Eq, Show)


data PipeKind =
    KindAudio
  | KindVideo
  | KindFile !ByteString
  | KindFilesystem
  | KindImage
  | KindPipe !ByteString
  | KindClipboard
  deriving (Eq, Show)


data GuacamoleStatusEnum =
    StatusSuccess
    -- ^ The operation succeeded.
  | StatusUnsupported
    -- ^ The requested operation is unsupported.
  | StatusServer !GuacamoleServerStatus
  | StatusClient !GuacamoleClientStatus
  | StatusUnknown !Int
  deriving (Eq, Show)

instance GuacamoleEnumEncoding GuacamoleStatusEnum where
    toGuacamoleEnum = \case
        0x0 ->   StatusSuccess
        0x100 -> StatusUnsupported
        x | 0x200 <= x && x > 0x300 -> StatusServer $ toEnum (x - 0x200)
        x | 0x300 <= x && x > 0x400 -> StatusClient $ toGuacamoleEnum x
        x -> StatusUnknown x

    fromGuacamoleEnum = \case
        StatusSuccess -> 0x00
        StatusUnsupported -> 0x100
        StatusServer s -> 0x200 + fromEnum s
        StatusClient c -> fromGuacamoleEnum c
        StatusUnknown x -> x


data GuacamoleCapStyle =
    GUAC_LINE_CAP_BUTT
  | GUAC_LINE_CAP_ROUND
  | GUAC_LINE_CAP_SQUARE
  deriving (Eq, Show, Enum)

data GuacamoleJoinStyle =
    GUAC_LINE_JOIN_BEVEL
  | GUAC_LINE_JOIN_MITER
  | GUAC_LINE_JOIN_ROUND
  deriving (Eq, Show, Enum)

data GuacamoleCompositeMode =
-- * A: Source where destination transparent = S n D'
-- * B: Source where destination opaque      = S n D
-- * C: Destination where source transparent = D n S'
-- * D: Destination where source opaque      = D n S
-- *
-- * 0 = Active, 1 = Inactive
--                               -- ABCD --
    GUAC_COMP_CLEAR     -- 0x00  /* 0000 - Clear (Unimplemented in client)         */
  | GUAC_COMP_RIN       -- 0x01  /* 0001 */
  | GUAC_COMP_ROUT      -- 0x02  /* 0010 - Clears destination where source opaque  */
  | GUAC_COMP_NOP       -- 0x03  /* 0011 - No operation (Unimplemented in client)  */
  | GUAC_COMP_IN        -- 0x04  /* 0100 */
  | GUAC_COMP_IN_ADD    -- 0x05  /* 0101 - Additive IN  (Unimplemented in client)  */
  | GUAC_COMP_ATOP      -- 0x06  /* 0110 - Fill where destination opaque only      */
  | GUAC_COMP_ATOP_ADD  -- 0x07  /* 0111 - Additive ATOP (Unimplemented in client) */
  | GUAC_COMP_OUT       -- 0x08, /* 1000 */
  | GUAC_COMP_RATOP     -- 0x09, /* 1001 */
  | GUAC_COMP_XOR       -- 0x0A  /* 1010 - XOR                                     */
  | GUAC_COMP_ROVER     -- 0x0B  /* 1011 - Fill where destination transparent only */
  | GUAC_COMP_SRC       -- 0x0C  /* 1100 */
  | GUAC_COMP_RATOP_ADD -- 0x0D  /* 0111 - Additive ATOP (Unimplemented in client) */
  | GUAC_COMP_OVER      -- 0x0E  /* 1110 - Draw normally                           */
  | GUAC_COMP_PLUS      -- 0x0F  /* 1111 - Add                                     */
  deriving (Eq, Show, Enum)
--    /* Bitwise composite operations (binary) */
--    /*
--     * A: S' & D'
--     * B: S' & D
--     * C: S  & D'
--     * D: S  & D
--     *
--     * 0 = Active, 1 = Inactive
--     */

data GuacamoleTransfer =
    --  Constant functions                    /* ABCD */
    GUAC_TRANSFER_BINARY_BLACK      -- = 0x0, /* 0000 */
    -- AND  --
 |  GUAC_TRANSFER_BINARY_AND    --     = 0x1, /* 0001 */
    -- AND with inverted destination
 |  GUAC_TRANSFER_BINARY_NDEST_AND  -- = 0x2, /* 0010 */
 -- GUAC_TRANSFER_BINARY_NSRC_NOR   -- = 0x2, /* 0010 */
    -- Copy functions
 |  GUAC_TRANSFER_BINARY_SRC    --     = 0x3, /* 0011 */
    -- AND / NAND with inverted source --
 |  GUAC_TRANSFER_BINARY_NSRC_AND   -- = 0x4, /* 0100 */
    -- NOR with inverted destination --
 -- GUAC_TRANSFER_BINARY_NDEST_NOR  -- = 0x4  /* 0100 */
    -- Copy functions
 |  GUAC_TRANSFER_BINARY_DEST   --     = 0x5, /* 0101 */
    -- XOR / XNOR --
 |  GUAC_TRANSFER_BINARY_XOR    --     = 0x6, /* 0110 */
    -- OR / NOR --
 |  GUAC_TRANSFER_BINARY_OR     --     = 0x7, /* 0111 */
 |  GUAC_TRANSFER_BINARY_NOR    --     = 0x8, /* 1000 */
    -- XOR / XNOR --
 |  GUAC_TRANSFER_BINARY_XNOR   --     = 0x9, /* 1001 */
    -- Copy functions
 |  GUAC_TRANSFER_BINARY_NDEST  --     = 0xA, /* 1010 */
    -- NAND with inverted source --
 |  GUAC_TRANSFER_BINARY_NSRC_NAND  -- = 0xB, /* 1011 */
    -- OR with inverted destination --
 -- GUAC_TRANSFER_BINARY_NDEST_OR   -- = 0xB, /* 1011 */
    -- Copy functions
 |  GUAC_TRANSFER_BINARY_NSRC   --     = 0xC, /* 1100 */

    -- OR / NOR with inverted source --
 |  GUAC_TRANSFER_BINARY_NSRC_OR    -- = 0xD, /* 1101 */
    -- NAND with inverted destination
 -- GUAC_TRANSFER_BINARY_NDEST_NAND -- = 0xD, /* 1101 */
    -- NAND --
 |  GUAC_TRANSFER_BINARY_NAND   --     = 0xE, /* 1110 */
    --  Constant functions
 |  GUAC_TRANSFER_BINARY_WHITE  --     = 0xF, /* 1111 */
 deriving (Eq, Show, Enum)

instance GuacamoleEnumEncoding GuacamoleJoinStyle
instance GuacamoleEnumEncoding GuacamoleCapStyle
instance GuacamoleEnumEncoding GuacamoleTransfer
instance GuacamoleEnumEncoding GuacamoleCompositeMode

