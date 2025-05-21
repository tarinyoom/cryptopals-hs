module Challenge1 where

import Data.Word (Word8)
import Data.Char (digitToInt, isHexDigit)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))

decodePair :: Char -> Char -> Word8
decodePair l r =
    let l_int = fromIntegral (digitToInt l)
        r_int = fromIntegral (digitToInt r)
    in (shiftL l_int 4) + r_int

-- Ignores dangling hex digit
hexStringToBytes :: String -> [Word8]
hexStringToBytes [] = []
hexStringToBytes [_] = []
hexStringToBytes (c1:c2:rest) =
    decodePair c1 c2 : hexStringToBytes rest

base64Chars :: String
base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

encodeTriple :: Word8 -> Word8 -> Word8 -> String
encodeTriple a b c =
    let i  = shiftL (fromIntegral a) 16
          .|. shiftL (fromIntegral b) 8
          .|. fromIntegral c
        c1 = base64Chars !! ((shiftR i 18) .&. 63)
        c2 = base64Chars !! ((shiftR i 12) .&. 63)
        c3 = base64Chars !! ((shiftR i 6)  .&. 63)
        c4 = base64Chars !! (i .&. 63)
    in [c1, c2, c3, c4]

bytesToBase64String :: [Word8] -> String
bytesToBase64String []         = []
bytesToBase64String [a]        = take 2 (encodeTriple a 0 0) ++ "=="
bytesToBase64String [a, b]     = take 3 (encodeTriple a b 0) ++ "="
bytesToBase64String (a:b:c:xs) = encodeTriple a b c ++ bytesToBase64String xs
