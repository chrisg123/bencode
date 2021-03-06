module BencodeTests where

import Bencode
import Test.HUnit
import Data.Map 

testEncodeInt :: Test
testEncodeInt =  TestCase $assertEqual "Should return bencoded integer" "i123e" (encode (BInt 123))
testDecodeInt :: Test
testDecodeInt =
  TestCase $ assertEqual "Should return a Just Int" (Just 3) (decodeInt "i3e")

testDecodeNegInt :: Test
testDecodeNegInt =
  TestCase $ assertEqual "Should return a negative Just Int" (Just (-3)) (decodeInt "i-3e")

testDecodeInvalidInt1 :: Test
testDecodeInvalidInt1 =
  TestCase $ assertEqual "Should return Nothing for only negative sign" Nothing (decodeInt "i-e")  

testDecodeInvalidInt2 :: Test
testDecodeInvalidInt2 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "i3-2e")  

testDecodeInvalidInt3 :: Test
testDecodeInvalidInt3 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "32e")

testDecodeInvalidInt4 :: Test
testDecodeInvalidInt4 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "i32")

testDecodeInvalidInt5 :: Test
testDecodeInvalidInt5 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "i-ae")

testDecodeInvalidInt6 :: Test
testDecodeInvalidInt6 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "iae")

testDecodeInvalidInt7 :: Test
testDecodeInvalidInt7 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "") 

testDecodeInvalidInt8 :: Test
testDecodeInvalidInt8 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "i")

testDecodeInvalidInt9 :: Test
testDecodeInvalidInt9 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "i-")

testDecodeInvalidInt10 :: Test
testDecodeInvalidInt10 =
  TestCase $ assertEqual "Should return Nothing for invalid bencoded integer" Nothing (decodeInt "i3")  

testEncodeStr :: Test
testEncodeStr =  TestCase $assertEqual "Should return bencoded integer" "5:hello" (encode (BStr "hello"))

testDecodeStr :: Test
testDecodeStr =
  TestCase $
  assertEqual
    "Should return a Just String"
    (Just "hello")
    (decodeStr "5:hello")

testDecodeSeqIntOrStr :: Test
testDecodeSeqIntOrStr =
  TestCase $
  assertEqual
    "Should return list of BValues of Int or String"
    (Just [BStr "hello", BStr "world!", BInt 123])
    (decodeSeq "5:hello6:world!i123e" [])

testDecodeLst1 :: Test
testDecodeLst1 =
  TestCase $
  assertEqual
    "Should return a list"
    (Just [BStr "hello", BStr "world!", BInt 123])
    (decodeLst "l5:hello6:world!i123ee")

testDecodeLst2 :: Test
testDecodeLst2 =
  TestCase $
  assertEqual
    "Should return a nested list"
    (Just [BLst [BStr "hello"]])
    (decodeLst "ll5:helloee")

testEncodeLst1 :: Test
testEncodeLst1 =
  TestCase $
  assertEqual
    "Should return encoded list."
  "l5:hello6:world!e"
    (encode (BLst [BStr "hello", BStr "world!"]))

testEncodeLst2 :: Test
testEncodeLst2 =
  TestCase $
  assertEqual
    "Should return encoded list."
  "l5:hello6:world!li123e3:abcee"
    (encode (BLst [BStr "hello", BStr "world!", BLst [BInt 123, BStr "abc"]]))

testDecodeLst :: Test
testDecodeLst =
  TestLabel
    "Test pairs."
    (TestList [testDecodeLst1, testDecodeLst2])

    
testDecodeDct1 :: Test
testDecodeDct1 = TestCase $ assertEqual "Should return nothing for invalid dictionary" Nothing (decodeDct "")

testDecodeDct2 :: Test
testDecodeDct2 = TestCase $ assertEqual "Should return nothing for invalid dictionary" Nothing (decodeDct "d")

testDecodeDct3 :: Test
testDecodeDct3 = TestCase $ assertEqual "Should return nothing for invalid dictionary" Nothing (decodeDct "d5:helloe")

testDecodeDct4 :: Test
testDecodeDct4 =
  TestCase $
  assertEqual
    "Sould return a Data.Map.fromList"
    (Just (Data.Map.fromList [("hello", BStr "world!")]))
    (decodeDct "d5:hello6:world!e")

testDecodeDct5 :: Test
testDecodeDct5 =
  TestCase $
  assertEqual
    "Sould return nested Data.Map.fromList"
    (Just (Data.Map.fromList [("key", BDct (Data.Map.fromList [("hello", BStr "world!")]))]))
    (decodeDct "d3:keyd5:hello6:world!ee")

testDecodeDct6 :: Test
testDecodeDct6 =
  TestCase $
  assertEqual
    "Sould return nested Data.Map.fromList with nested list"
    (Just (Data.Map.fromList [("key", BDct (Data.Map.fromList [("hello", BLst [BStr "world!", BInt 123])]))]))
    (decodeDct "d3:keyd5:hellol6:world!i123eeee")

testEncodeDct :: Test
testEncodeDct =
  TestCase $
  assertEqual
    "Should return a bencoded dictionary."
    "d4:key16:value14:key26:value2e"
    (encode (BDct (fromList [("key1", BStr "value1"), ("key2", BStr "value2")])))

testDecodeDct :: Test
testDecodeDct =
  TestLabel
    "Test pairs."
    (TestList
       [ testDecodeDct1
       , testDecodeDct2
       , testDecodeDct3
       , testDecodeDct4
       , testDecodeDct5
       , testDecodeDct6
       ])

testDecode :: Test
testDecode = TestLabel "Test decode." (   TestList
    [ testDecodeInt
    , testDecodeNegInt
    , testDecodeInvalidInt1
    , testDecodeInvalidInt2
    , testDecodeInvalidInt3
    , testDecodeInvalidInt4
    , testDecodeInvalidInt5
    , testDecodeInvalidInt6
    , testDecodeInvalidInt7
    , testDecodeInvalidInt8
    , testDecodeInvalidInt9
    , testDecodeInvalidInt10    
    , testDecodeStr
    , testDecodeSeqIntOrStr
    , testDecodeLst
    , testDecodeDct
    ] )


testEncode :: Test
testEncode = TestLabel "Test encode." (   TestList
    [ testEncodeInt
    , testEncodeLst1
    , testEncodeLst2
    , testEncodeDct
    ] )

main :: IO Counts
main = runTestTT $ TestList [testEncode, testDecode]
