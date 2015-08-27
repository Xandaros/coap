module Data.Coap.Internal where

class XEnum a where
  toxEnum :: Int -> Either String a
  fromxEnum :: a -> Int
