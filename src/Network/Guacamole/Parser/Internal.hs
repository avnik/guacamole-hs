{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Network.Guacamole.Parser.Internal (
                                           Parser (..)
                                         , runParser
                                         , chunk
                                         , failure
                                         , integral
                                         , float
                                         , text
                                         , unicode
    ) where

import           Universum

import           Control.Applicative (Alternative (..))
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Trans.Except (Except, ExceptT, runExcept, throwE)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString
import           Data.Scientific (Scientific, floatingOrInteger, toBoundedInteger, toRealFloat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable (Typeable)
import           GHC.Float (RealFloat)

data Input
    = Arguments { position :: !Integer, source :: [ByteString] }


newtype Parser a =
  Parser ( StateT Input ( Except Text ) a )
  deriving ( Functor , Applicative , Alternative , Monad , MonadPlus , MonadError Text )


-- |
-- Apply a parser to arguments.
runParser :: Parser a -> [ByteString] -> Either Text a
runParser (Parser parser) input =
    runExcept (evalStateT parser $ Arguments 1 input)
{-# INLINE runParser #-}

-- |
-- Fail with a message.
failure :: Text -> Parser a
failure msg =
    Parser (lift (throwE msg))
{-# INLINE failure #-}

instance MonadFail Parser where
    fail e = Parser $ lift $ throwE $ T.pack e

chunk :: Parser ByteString
chunk = Parser $ StateT $ \(Arguments pos src) ->
    case src  of
      []       -> throwE $ T.pack  ("Not enough input at " <>  show  pos)
      (x : xs) -> pure (x, Arguments (succ  pos) xs)

scientific :: Parser Scientific
scientific = chunk >>= \bs -> case A.parseOnly (A.scientific <* A.endOfInput) bs of
                                Right sci -> pure sci
                                Left e    -> fail e

integralFromScientific :: Integral a => Scientific -> Parser a
integralFromScientific s =
    case floatingOrInteger s :: Integral a => Either Double a of
        Right x -> pure x
        Left _  -> fail $ "expected integer, encountered floating number " ++ show s
{-# INLINE integralFromScientific #-}


parseBoundedIntegralFromScientific :: (Bounded a, Integral a) => String -> Scientific -> Parser a
parseBoundedIntegralFromScientific expected s = maybe
    (fail $ expected ++ " is either floating or will cause over or underflow: " ++ show s)
    pure
    (toBoundedInteger s)
{-# INLINE parseBoundedIntegralFromScientific #-}

integral :: Integral a => Parser a
integral = scientific >>= integralFromScientific

float :: RealFloat a => Parser a
float = toRealFloat <$> scientific

text :: Parser Text
text = T.decodeLatin1 <$> chunk

unicode :: Parser Text
unicode = chunk >>= \bs -> case T.decodeUtf8' bs of
                             Left e  -> fail $ "unicode error: " ++ show e
                             Right t -> pure t
