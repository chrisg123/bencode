 module Bencode where
                
isDigit :: Char -> Bool
isDigit x = x `elem` ['0' .. '9']

takeDigits :: String -> String
takeDigits = takeWhile isDigit

lastIndex :: Foldable t => t a-> Int
lastIndex x = length x - 1
 
-- |Find the first accurance of a bencoded element in a string.
bfind :: String -> String
bfind s =
  let (i, len) = bfindPos s 0
   in take len (drop i s)

-- |Find the position of the first accurance of a bencoded element in a string.
bfindPos :: String -> Int -> (Int, Int)
bfindPos "" _ = (0, 0)
bfindPos s i
  | i >= lastIndex s = (lastIndex s, 0)
  | otherwise =
    let b = takeB $ drop (min i $ lastIndex s) s
     in if b /= ""
          then (i, length b)
          else bfindPos s (i + 1)

-- |Take a bencoded element from the head of a string.
takeB :: String -> String
takeB "" = ""
takeB (x:xs)
  | isDigit x = takeBstr (x : xs)
  | x == 'i' = takeBint (x : xs)
  | x == 'l' || x == 'd' = takeBCollection (x : xs)
  | otherwise = ""

-- |Take all consecutive bencoded elements from the head of a string.
takeBs :: String -> [String]
takeBs "" = []
takeBs s =
  let b = takeB s
   in if b == ""
        then []
        else b : takeBs (drop (length b) s)

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
          then take (metaLength + length ":" + dataLength) (x : xs)
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
          then take (dataLength + length (x : "e")) (x : xs)
          else ""
  | otherwise = ""

-- |Take a bencoded list or dictionary element from the head of a string.
takeBCollection :: String -> String
takeBCollection "" = ""
takeBCollection (x:xs)
  | x == 'l' || x == 'd' =
    let bs = concat $ takeBs xs
        bsLength = length bs
        validSuffix = (x : xs) !! (1 + bsLength) == 'e'
     in if validSuffix
          then take (bsLength + length (x : "e")) (x : xs)
          else ""
  | otherwise = "" 
