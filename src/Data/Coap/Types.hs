{-# LANGUAGE TemplateHaskell #-}
module Data.Coap.Types where
import Data.ByteString
import Data.Word

import Control.Lens

import Data.Coap.Internal.TH

declareLenses [d|
    data Message = Message { messageHeader :: !MessageHeader
                           , code          :: !Code
                           , messageId     :: !Word16
                           , token         :: !ByteString
                           , option        :: ![Option]
                           , payload       :: !ByteString
                           }

    data MessageHeader = MessageHeader { version     :: !Word8
                                       , typ         :: !Type
                                       , tokenLength :: !Word8
                                       }
    
    data Type = CON
              | NON
              | ACK
              | RST
              deriving (Eq)
    
    data Code = MethodCode MethodCode
              | ResponseCode ResponseCode
              deriving (Eq)
    
    data MethodCode = GET
                    | PUT
                    | POST
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
    
    data Option = Option
    
    data ClientErrorCode = CECPH
                         deriving (Show, Eq)
    
    data ServerErrorCode = SECPH
                         deriving (Show, Eq)
    |]

generateEnum ''Type 0
generateEnum ''MethodCode 1
generateEnum ''SuccessCode 1
generateEnum ''ClientErrorCode 1
generateEnum ''ServerErrorCode 0
