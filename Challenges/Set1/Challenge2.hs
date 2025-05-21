module Challenge2 where

import Data.Char (digitToInt)
import Data.Word (Word8)
import Data.Bits (shiftL, shiftR, (.&.), xor)

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

nibbleToHex :: Int -> Char
nibbleToHex n
    | n >= 0 && n <= 9  = toEnum (n + fromEnum '0')
    | n >= 10 && n <= 15 = toEnum (n - 10 + fromEnum 'a')
    | otherwise = error "Invalid nibble"

encodeByte :: Word8 -> (Char, Char)
encodeByte b =
    let i = fromIntegral b
        hi = nibbleToHex (i `shiftR` 4)
        lo = nibbleToHex (i .&. 0xF)
    in (hi, lo)

bytesToHexString :: [Word8] -> String
bytesToHexString [] = []
bytesToHexString (c:rest) =
    let (hi, lo) = encodeByte c
    in hi:lo:bytesToHexString rest

bytesXOR :: [Word8] -> [Word8] -> [Word8]
bytesXOR (l:restl) (r:restr) =
  (xor l r : bytesXOR restl restr)
bytesXOR _ _ = []

fixedXOR :: String -> String -> String
fixedXOR l r =
    let bytes_l = hexStringToBytes l
        bytes_r = hexStringToBytes r
        bytes = bytesXOR bytes_l bytes_r
    in bytesToHexString bytes
