 module Bencode where

-- |Find the first accurance of a bencoded element in a string.
bfind :: String -> String
bfind s =
  let (i, len) = bfindPos s 0
   in take len (drop i s)

-- |Find the position of the first accurance of a bencoded element in a string.
bfindPos :: String -> Int -> (Int, Int)
bfindPos "" i = (max 0 (i - 1), 0)
bfindPos (x:xs) i =
  let b = takeB (x : xs)
   in if b /= "" then (i, length b) else bfindPos xs (i + 1)

-- |Take a bencoded element from the head of a string.
takeB :: String -> String
takeB "" = ""
takeB (x:xs)
  | isDigit x = takeBstr (x:xs)
  | x == 'i' = takeBint (x:xs)
  | otherwise = ""

-- |Take a bencoded string element from the head of a string.
takeBstr :: String -> String
takeBstr "" = ""
takeBstr (x:xs)
  | isDigit x =
    let metaString = takeDigits (x : xs)
        metaLength = length metaString
        dataLength = read metaString :: Int
        validSuffix = (x : xs) !! metaLength == ':'
     in if validSuffix
          then take  (metaLength + length ":" + dataLength) (x : xs)
          else ""
  | otherwise = ""

-- |Take a bencoded integer element from the head of a string.
takeBint :: String -> String
takeBint "" = ""
takeBint (x:xs)
  | x == 'i' =
    let dataString = takeDigits xs
        dataLength = length dataString
        validSuffix = (x : xs) !! (1 + dataLength) == 'e'
     in if validSuffix
          then take (dataLength + length "ie") (x:xs)
          else ""
  | otherwise = ""

-- |Take a bencoded list element from the head of a string.
takeBlist :: String -> String
takeBlist "" = ""
takeBlist (x:xs) = undefined
                
isDigit :: Char -> Bool
isDigit x = x `elem` ['0' .. '9']

takeDigits :: String -> String
takeDigits = takeWhile isDigit
