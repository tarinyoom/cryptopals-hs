module Main (main) where

import Test.HUnit
import qualified Challenge1
import qualified Challenge2

challenge1 :: Test
challenge1 = "Verify Challenge 1" ~:
    (let input    = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
         expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
         actual   = Challenge1.bytesToBase64String (Challenge1.hexStringToBytes input)
    in expected ~?= actual)

challenge2 :: Test
challenge2 = "Verify Challenge 2" ~:
    (let input1   = "1c0111001f010100061a024b53535009181c"
         input2   = "686974207468652062756c6c277320657965"
         expected = "746865206b696420646f6e277420706c6179"
         actual   = Challenge2.fixedXOR input1 input2
    in expected ~?= actual)

main :: IO ()
main = runTestTTAndExit $ TestList [challenge1, challenge2]
