module BencodeTests where

import Bencode(bfind, bfindR)
import Test.HUnit

testBfindREmpty :: Test
testBfindREmpty = TestCase $ assertEqual "Should return (0,0) for empty string." (0,0) (bfindR "" 0)

testBfindEmpty :: Test
testBfindEmpty = TestCase $ assertEqual "Should return empty string given an empty string." "" (bfind "")

testBfindRNoMatch :: Test
testBfindRNoMatch =
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

testBfindRFuzzyMatch :: Test
testBfindRFuzzyMatch =
  TestCase $
  assertEqual
    "Should find bencoded string within a malformed bencoded string."
    (4, 7)
    (bfindR "asdf5:hello" 0)

testBfindFuzzyMatch :: Test
testBfindFuzzyMatch =
  TestCase $
  assertEqual
    "Should find bencoded string within a malformed bencoded data."
    "5:hello"
    (bfind "asdf5:hello")

testBfindRNoColon :: Test
testBfindRNoColon =
  TestCase $
  assertEqual
    "Should skip forward if number not followed by colon."
    (10, 8)
    (bfindR "asdf5Hello6:World!" 0)

testBfindNoColon :: Test
testBfindNoColon =
  TestCase $
  assertEqual
    "Should skip forward if number not followed by colon."
    "6:World!"
    (bfind "asdf5Hello6:World!")    

testBfindRFirstWord :: Test
testBfindRFirstWord =
  TestCase $
  assertEqual
    "Should only find the first word encountered."
    (0, 7)
    (bfindR "5:Hello6:World!" 0)

testBfindFirstWord :: Test
testBfindFirstWord =
  TestCase $
  assertEqual
    "Should only find the first word encountered."
    "5:Hello"
    (bfind "5:Hello6:World!")

testBfindR :: Test
testBfindR = TestLabel "Test bfindR." (  TestList
    [ testBfindREmpty
    , testBfindRNoMatch
    , testBfindRFuzzyMatch
    , testBfindRNoColon
    , testBfindRFirstWord
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
main = runTestTT $ TestList [testBfindR, testBfind]
