module Bencode where

data BType
  = BTypeStr
  | BTypeInt
  | BTypeList
  | BTypeDict
  deriving (Eq, Show)

data BValue
  = BStr String
  | BInt Int
  | BList [BValue]
  | BDict [(String, BValue)]

isDigit :: Char -> Bool
isDigit x = x `elem` ['0' .. '9']

takeDigits :: String -> String
takeDigits = takeWhile isDigit

lastIndex :: Foldable t => t a-> Int
lastIndex x = length x - 1

getBType :: String -> Maybe BType
getBType "" = Nothing
getBType (x:xs)
  | takeB (x : xs) == "" = Nothing
  | otherwise =
    let f 'i' = BTypeInt
        f 'l' = BTypeList
        f 'd' = BTypeDict
        f _ = BTypeStr
     in Just (f x)

getBValue :: String -> Maybe BValue
getBValue "" = Nothing
getBValue (x:xs) =
  let b = takeB (x : xs)
   in if b == ""
        then Nothing
        else Just (BStr b)
      
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

takeB2 :: String -> Maybe (BType, String)
takeB2 "" = Nothing
takeB2 (x:xs)
  | isDigit x =
    let v = takeBstr (x : xs)
     in if v == ""
          then Nothing
          else Just (BTypeStr, v)
  | x == 'i' =
    let v = takeBint (x : xs)
     in if v == ""
          then Nothing
          else Just (BTypeInt, v)
  | x == 'l' =
    let v = takeBCollection (x : xs)
     in if v == ""
          then Nothing
          else Just (BTypeList, v)
  | x == 'd' =
    let v = takeBCollection (x : xs)
     in if v == ""
          then Nothing
          else Just (BTypeDict, v)
  | otherwise = Nothing


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
