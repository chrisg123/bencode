module BencodeTests where

import Bencode(bfind, bfindPos)
import Test.HUnit

testBfindPosEmpty :: Test
testBfindPosEmpty = TestCase $ assertEqual "Should return (0,0) for empty string." (0,0) (bfindPos "" 0)

testBfindEmpty :: Test
testBfindEmpty = TestCase $ assertEqual "Should return empty string given an empty string." "" (bfind "")

testBfindPosNoMatch :: Test
testBfindPosNoMatch =
  TestCase $
  assertEqual
    "Should return empty string for no match in string."
    ""
    (bfind "asdf")

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

testBfindPos :: Test
testBfindPos = TestLabel "Test bfindPos." (  TestList
    [ testBfindPosEmpty
    , testBfindPosNoMatch
    , testBfindPosFuzzyMatch
    , testBfindPosNoColon
    , testBfindPosFirstWord
    , testBfindPosInt
    , testBfindPosFuzzyMatchInt
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
main = runTestTT $ TestList [testBfindPos, testBfind]
