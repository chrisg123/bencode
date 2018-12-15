module BencodeTests where

import Bencode
import Test.HUnit

testEncodeInt :: Test
testEncodeInt =  TestCase $assertEqual "Should return bencoded integer" "i123e" (encodeInt 123
                                                                                )
testDecodeInt :: Test
testDecodeInt =
  TestCase $ assertEqual "Should return a Just Int" (Just 3) (decodeInt "i3e")

testDecodeNegInt :: Test
testDecodeNegInt =
  TestCase $ assertEqual "Should return a negative Just Int" (Just (-3)) (decodeInt "i-3e")
  
testEncodeStr :: Test
testEncodeStr =  TestCase $assertEqual "Should return bencoded integer" "5:hello" (encodeStr "hello")

testDecodeStr :: Test
testDecodeStr =
  TestCase $
  assertEqual
    "Should return a Just String"
    (Just "hello")
    (decodeStr "5:hello")

testDecodeIntOrStr1 :: Test
testDecodeIntOrStr1 =
    TestCase $
  assertEqual
    "Should return a Just String"
    (Just $ BStr "hello")
    (decodeIntOrStr "5:hello")

testDecodeIntOrStr2 :: Test
testDecodeIntOrStr2 =
  TestCase $
  assertEqual "Should return a Just Int" (Just $ BInt 3) (decodeIntOrStr "i3e")

testDecodeSeqIntOrStr :: Test
testDecodeSeqIntOrStr =
  TestCase $
  assertEqual
    "Should return list of BValues of Int or String"
    (Just [BStr "hello", BStr "world!", BInt 123])
    (decodeSeqIntOrStr "5:hello6:world!i123e" [])

testDecodeLst :: Test
testDecodeLst =
  TestCase $
  assertEqual
    "Should return a list"
    (Just [BStr "hello", BStr "world!", BInt 123])
    (decodeLst "l5:hello6:world!i123ee")

testDecode :: Test
testDecode = TestLabel "Test decode." (   TestList
    [ testEncodeInt
    , testDecodeInt
    , testDecodeNegInt    
    , testDecodeStr
    , testDecodeIntOrStr1
    , testDecodeIntOrStr2
    , testDecodeSeqIntOrStr
    , testDecodeLst
    ] )

main :: IO Counts
main = runTestTT $ TestList [testDecode]
