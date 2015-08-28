module Data.Coap ( module Data.Coap.Types
                 , XEnum(..)
                 , parseMessage
                 ) where

import qualified Data.ByteString.Lazy as BS
import Debug.Trace (traceShowId, trace)
import Data.Word

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Data.Binary.Get as Bytes
import Data.Binary.Bits.Get as Bits ( word8
                                    , getWord8
                                    , BitGet
                                    , runBitGet
                                    , block
                                    , Block
                                    )

import Data.Coap.Types
import Data.Coap.Internal

type ET = EitherT String

runOrFail :: BS.ByteString -> ET Get Message -> Either String Message
runOrFail dat = join . bimap getThird getThird . flip runGetOrFail dat . runEitherT
  where
    getThird (a,b,c) = c

runBG :: ET BitGet a -> ET Get a
runBG = EitherT . runBitGet . runEitherT

parseMessage :: BS.ByteString -> Either String Message
parseMessage str = runOrFail str messageParser
  where
    getThird (a,b,c) = c

messageParser :: ET Get Message
messageParser = do
  header <- runBG headerBlock
  preMsg <- Message <$> return header
                    <*> runBG parseCode
                    <*> lift getWord16be
                    <*> lift (getLazyByteString (fromIntegral $ header^.tokenLength))
  (opts, plPresent) <- parseOptions
  empty <- lift isEmpty
  pl <- if plPresent && empty
          then left "Message format error: Options terminated but no payload"
          else lift (skip 1 >> getRemainingLazyByteString)
  lift . return $ preMsg opts pl

headerBlock :: ET BitGet MessageHeader
headerBlock =
  MessageHeader <$> lift (Bits.getWord8 2)
                <*> typeBlock
                <*> lift (Bits.getWord8 4)

typeBlock :: ET BitGet Type
typeBlock = EitherT $ toxEnum . fromIntegral <$> Bits.getWord8 2

  --toxEnum . fromIntegral <$> word8 2

parseCode :: ET BitGet Code
parseCode = do
  clazz <- lift $ fromIntegral <$> Bits.getWord8 3
  detail <- lift $ fromIntegral <$> Bits.getWord8 5
  case clazz of
    0 -> MethodCode <$> (hoistEither . toxEnum $ detail)
    2 -> ResponseCode . SuccessCode <$> (hoistEither . toxEnum $ detail)
    4 -> ResponseCode . ClientErrorCode <$> (hoistEither . toxEnum $ detail)
    5 -> ResponseCode . ServerErrorCode <$> (hoistEither . toxEnum $ detail)
    a -> error $ "Invalid code: " ++ show a

parseOptions :: ET Get ([DOption], Bool)
parseOptions = do
  empty <- lift isEmpty
  if empty
    then return ([], False)
    else do
      firstByte <- lift $ lookAhead Bytes.getWord8
      if firstByte == 0xFF
        then return ([], True)
        else do
          option <- parseOption
          (nextOpts, f) <- parseOptions
          return (option:nextOpts, f)

parseOption :: ET Get DOption
parseOption = do
  (delta, length) <- runBG parseOptionHeader
  delta' <- case delta of
    15 -> left "Message format error: Invalid option delta"
    14 -> lift $ ((+269) . fromIntegral) <$> getWord16be
    13 -> lift $ ((+13)  . fromIntegral) <$> Bytes.getWord8
    _  -> return $ fromIntegral delta
  length' <- case length of
    15 -> left "Message format error: Invalid option length"
    14 -> lift $ ((+269) . fromIntegral) <$> getWord16be
    13 -> lift $ ((+13)  . fromIntegral) <$> Bytes.getWord8
    _  -> return $ fromIntegral length
  value <- lift $ getLazyByteString (fromIntegral (trace ("Length: " ++ show length') length'))
  return (DOption delta' length' value)

parseOptionHeader :: ET BitGet (Word8, Word8)
parseOptionHeader = do
  delta  <- lift $ Bits.getWord8 4
  length <- lift $ Bits.getWord8 4
  return (delta, length)
