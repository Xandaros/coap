module Data.Coap ( module Data.Coap.Types
                 ) where

import qualified Data.ByteString.Lazy as BS

import Control.Lens

import Data.Binary.Get
import Data.Binary.Bits.Get as Bits ( word8
                                    , getWord8
                                    , BitGet
                                    , runBitGet
                                    , block
                                    , Block
                                    )

import Data.Coap.Types

parseMessage :: BS.ByteString -> Message
parseMessage = runGet messageParser

messageParser :: Get Message
messageParser = do
  header <- runBitGet (block headerBlock)
  Message <$> return header
          <*> runBitGet parseCode
          <*> getWord16be
          <*> getByteString (fromIntegral $ header^.tokenLength)
          <*> return []
          <*> return mempty

headerBlock :: Block MessageHeader
headerBlock =
  MessageHeader <$> word8 2
                <*> typeBlock
                <*> word8 4

typeBlock :: Block Type
typeBlock = toEnum . fromIntegral <$> word8 2

parseCode :: BitGet Code
parseCode = do
  clazz <- fromIntegral <$> Bits.getWord8 3
  detail <- fromIntegral <$> Bits.getWord8 5
  return $ case clazz of
    0 -> MethodCode $ toEnum detail
    2 -> ResponseCode . SuccessCode $ toEnum detail
    4 -> ResponseCode . ClientErrorCode $ toEnum detail
    5 -> ResponseCode . ServerErrorCode $ toEnum detail
