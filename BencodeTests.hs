module BencodeTests where

import Bencode
import Test.HUnit

testBfindPosEmpty :: Test
testBfindPosEmpty = TestCase $ assertEqual "Should return (0,0) for empty string." (0,0) (bfindPos "" 0)

testBfindEmpty :: Test
testBfindEmpty = TestCase $ assertEqual "Should return empty string given an empty string." "" (bfind "")

testBfindPosNoMatch :: Test
testBfindPosNoMatch =
  TestCase $
  assertEqual
    "Should return (lastIndex str,0) for no match in string."
    (3,0)
    (bfindPos "asdf" 0)

testBfindNoMatch :: Test
testBfindNoMatch =
  TestCase $
  assertEqual
    "Should return empty for no match in string."
    ""
    (bfind "asdf")

testBfindPosFuzzyMatch :: Test
testBfindPosFuzzyMatch =
  TestCase $
  assertEqual
    "Should find bencoded string within a malformed bencoded string."
    (4, 7)
    (bfindPos "asdf5:hello" 0)

testBfindFuzzyMatch :: Test
testBfindFuzzyMatch =
  TestCase $
  assertEqual
    "Should find bencoded string within a malformed bencoded data."
    "5:hello"
    (bfind "asdf5:hello")

testBfindPosNoColon :: Test
testBfindPosNoColon =
  TestCase $
  assertEqual
    "Should skip forward if number not followed by colon."
    (10, 8)
    (bfindPos "asdf5Hello6:World!" 0)

testBfindNoColon :: Test
testBfindNoColon =
  TestCase $
  assertEqual
    "Should skip forward if number not followed by colon."
    "6:World!"
    (bfind "asdf5Hello6:World!")    

testBfindPosFirstWord :: Test
testBfindPosFirstWord =
  TestCase $
  assertEqual
    "Should only find the first word encountered."
    (0, 7)
    (bfindPos "5:Hello6:World!" 0)

testBfindFirstWord :: Test
testBfindFirstWord =
  TestCase $
  assertEqual
    "Should only find the first word encountered."
    "5:Hello"
    (bfind "5:Hello6:World!")

testBfindPosInt :: Test
testBfindPosInt =
  TestCase $
  assertEqual "Should find bencoded integer position." (0, 5) (bfindPos "i123e" 0)

testBfindPosFuzzyMatchInt :: Test
testBfindPosFuzzyMatchInt =
  TestCase $
  assertEqual
    "Should find bencoded integer position in malformed bencoded string."
    (4, 5)
    (bfindPos "asdfi123e" 0)

testBfindPosOutOfBound :: Test
testBfindPosOutOfBound =
  TestCase $
  assertEqual
    "Should return (lastIndex str, 0) when i >= length str."
    (14,0)
    (bfindPos "5:Hello6:World!" 15)
  
testBfindPosStartPos :: Test
testBfindPosStartPos =
  TestCase $
  assertEqual
    "Should start search at i."
    (7,8)
    (bfindPos "5:Hello6:World!" 1)

testBfindPosInvalidList :: Test
testBfindPosInvalidList =
  TestCase $
  assertEqual
    "Should return (lastIndex,0) for invalid list."
    (3,0)
    (bfindPos "lfda" 0)

testTakeBsTwo :: Test
testTakeBsTwo =
  TestCase $
  assertEqual
    "Should take two consecutive b encoded elements."
    ["5:Hello","6:World!"]
    (takeBs "5:Hello6:World!")

testBfindPosListOfTwo :: Test
testBfindPosListOfTwo =
  TestCase $
  assertEqual
    "Should return the position of a bencoded list of two elements."
    (0,17)
    (bfindPos "l5:Hello6:World!e" 0)

testBfindPosDict :: Test
testBfindPosDict =
  TestCase $
  assertEqual
    "Should return the position of a bencoded dictionary."
    (0,24)
    (bfindPos "d3:cow3:moo4:spam4:eggse" 0)


testGetBTypeNothing :: Test
testGetBTypeNothing =
  TestCase $
  assertEqual "Should return Nothing" Nothing (getBType "asdf")

testGetBTypeStr :: Test
testGetBTypeStr =
  TestCase $
  assertEqual "Should return BTypeStr" (Just BTypeStr) (getBType "5:hello")

testGetBTypeInt :: Test
testGetBTypeInt =
  TestCase $
  assertEqual "Should return BTypeInt" (Just BTypeInt) (getBType "i123e")

testGetBTypeList :: Test
testGetBTypeList =
  TestCase $
  assertEqual "Should return BTypeList" (Just BTypeList) (getBType "li123e5:helloe")

testGetBTypeDict :: Test
testGetBTypeDict =
  TestCase $
  assertEqual "Should return BTypeDict" (Just BTypeDict) (getBType "di123e5:helloe")


testGetBType :: Test
testGetBType =
  TestLabel
    "Test getBType"
    (TestList
       [ testGetBTypeStr
       , testGetBTypeInt
       , testGetBTypeList
       , testGetBTypeDict
       , testGetBTypeNothing
       ])

testBfindPos :: Test
testBfindPos = TestLabel "Test bfindPos." (  TestList
    [ testBfindPosEmpty
    , testBfindPosNoMatch
    , testBfindPosFuzzyMatch
    , testBfindPosNoColon
    , testBfindPosFirstWord
    , testBfindPosInt
    , testBfindPosFuzzyMatchInt
    , testBfindPosInvalidList
    , testBfindPosStartPos
    , testBfindPosOutOfBound
    , testBfindPosListOfTwo
    , testBfindPosDict
    ] )

testBfind :: Test
testBfind = TestLabel "Test bfind." (   TestList
    [ testBfindEmpty
    , testBfindNoMatch
    , testBfindFuzzyMatch
    , testBfindNoColon
    , testBfindFirstWord
    ] )

main :: IO Counts
main = runTestTT $ TestList [testTakeBsTwo, testBfindPos, testBfind, testGetBType]
