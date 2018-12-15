module Bencode where

data BType
  = BTypeStr
  | BTypeInt
  | BTypeList
  | BTypeDict
  | BTypeInvalid
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
getBType x =
  case takeTaggedB x of
    Just (t, _) -> Just t
    _ -> Nothing

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
takeB x =
  let f (Just (_, s)) = s
      f _ = ""
       in f $ takeTaggedB x

-- |Take a bencoded element from the head of a string and tag it with its type 
takeTaggedB :: String -> Maybe (BType, String)
takeTaggedB "" = Nothing;
takeTaggedB (x:xs)
  | isDigit x = Just $ f (BTypeStr, maybe "" encStr . decStr)
  | x == 'i' = Just $ f (BTypeInt, maybe "" encInt . decInt)
  | x == 'l' = Just $ f (BTypeList, takeBCollection)
  | x == 'd' = Just $ f (BTypeDict, takeBCollection)
  | otherwise = Nothing
  where
    f (p, q) = (p, q (x : xs))

-- |Take all consecutive bencoded elements from the head of a string.
takeBs :: String -> [String]
takeBs "" = []
takeBs s =
  let b = takeB s
   in if b == ""
        then []
        else b : takeBs (drop (length b) s)

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


encInt :: Int -> String
encInt x = "i" ++ show x ++ "e"

decInt :: String -> Maybe Int
decInt "" = Nothing
decInt (x:xs)
  | x == 'i' =
    let dataString = takeDigits xs
        validSuffix = (x : xs) !! (1 + length dataString) == 'e'
     in if validSuffix
          then Just $ read dataString
          else Nothing
  | otherwise  = Nothing
           
encStr :: String -> String
encStr x = show (length x) ++ ":" ++ x

decStr :: String -> Maybe String
decStr "" = Nothing
decStr (x:xs)
  | isDigit x =
    let metaString = takeDigits (x : xs)
        metaLength = length metaString
        dataLength = read metaString :: Int
        validSuffix = (x : xs) !! metaLength == ':'
     in if validSuffix
          then Just $ take dataLength (drop (metaLength + 1) (x:xs))
          else Nothing
  | otherwise = Nothing


-- decIntOrStr :: String -> Either Int String
