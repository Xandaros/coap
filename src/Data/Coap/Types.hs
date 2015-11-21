{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Coap.Types where
import Data.ByteString.Lazy
import Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word

import Control.Lens

import Data.Coap.Internal
import Data.Coap.Internal.TH

declareLenses [d|
    data Message = Message { messageHeader :: !MessageHeader
                           , code          :: !Code
                           , messageId     :: !Word16
                           , token         :: !ByteString
                           , option        :: ![RawOption]
                           , payload       :: !ByteString
                           } deriving (Show)

    data MessageHeader = MessageHeader { version     :: !Word8
                                       , typ         :: !Type
                                       , tokenLength :: !Word8
                                       } deriving (Show)
    
    data Type = CON
              | NON
              | ACK
              | RST
              deriving (Show, Eq)
    
    data Code = MethodCode MethodCode
              | ResponseCode ResponseCode
              deriving (Show, Eq)
    
    data MethodCode = EMPTY
                    | GET
                    | POST
                    | PUT
                    | DELETE
                    deriving (Show, Eq)
    
    data ResponseCode = SuccessCode SuccessCode
                      | ClientErrorCode ClientErrorCode
                      | ServerErrorCode ServerErrorCode
                      deriving (Show, Eq)
    
    data SuccessCode = Created
                     | Deleted
                     | Valid
                     | Changed
                     | Content
                     deriving (Show, Eq)
    
    data RawOption = RawOption { optionNo :: Word32
                               , length :: Word32
                               , value :: ByteString
                               }
                         deriving (Show)

    data Options = Options { ifMatch :: [ByteString]
                           , uriHost :: Maybe ByteString
                           , eTag :: [ByteString]
                           , ifNoneMatch :: Bool
                           , uriPort :: Maybe Word16
                           , locationPath :: [ByteString]
                           , uriPath :: [ByteString]
                           , contentFormat :: Maybe Word16
                           , maxAge :: Maybe Word32
                           , uriQuery :: [ByteString]
                           , accept :: Maybe Word16
                           , locationQuery :: [ByteString]
                           , proxyUri :: Maybe ByteString
                           , proxyScheme :: Maybe ByteString
                           , size1 :: Maybe Word32
                           , unknownOptions :: [RawOption]
                           } deriving (Show)

    data ClientErrorCode = CECPH
                         deriving (Show, Eq)
    
    data ServerErrorCode = SECPH
                         deriving (Show, Eq)
    |]

generateEnum ''Type 0
generateEnum ''MethodCode 0
generateEnum ''SuccessCode 1
generateEnum ''ClientErrorCode 1
generateEnum ''ServerErrorCode 0

defaultOptions = set ifMatch []
               . set uriHost Nothing
               . set eTag []
               . set ifNoneMatch False
               . set uriPort Nothing
               . set locationPath []
               . set uriPath []
               . set contentFormat Nothing
               . set maxAge Nothing
               . set uriQuery []
               . set accept Nothing
               . set locationQuery []
               . set proxyUri Nothing
               . set proxyScheme Nothing
               . set size1 Nothing
               . set unknownOptions []
               $ Options{}
