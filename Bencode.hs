module Bencode where
import Data.Char (isDigit)

data BValue
  = BStr String
  | BInt Int
  | BList [BValue]
  | BDict [(BValue, BValue)]
  deriving (Eq, Show)

encodeInt :: Int -> String
encodeInt x = "i" ++ show x ++ "e"

decodeInt :: String -> Maybe Int
decodeInt "" = Nothing
decodeInt (x:xs)
  | x /= 'i' = Nothing
  | otherwise =
    let dataString = takeWhile (\y -> isDigit y || y == '-') xs
        validSuffix = (x : xs) !! (1 + length dataString) == 'e'
     in if validSuffix
          then Just $ read dataString
          else Nothing
           
encodeStr :: String -> String
encodeStr x = show (length x) ++ ":" ++ x

decodeStr :: String -> Maybe String
decodeStr "" = Nothing
decodeStr (x:xs)
  | not $ isDigit x = Nothing
  | otherwise =
    let metaString = takeWhile isDigit (x : xs)
        metaLength = length metaString
        dataLength = read metaString :: Int
        validSuffix = (x : xs) !! metaLength == ':'
     in if validSuffix
          then Just $ take dataLength (drop (metaLength + 1) (x:xs))
          else Nothing

decodeIntOrStr :: String -> Maybe BValue
decodeIntOrStr x =
  case decodeInt x of
    Just y -> return $ BInt y
    _ -> decodeStr x >>= \y -> return $ BStr y

decodeSeqIntOrStr :: String -> [BValue] -> Maybe [BValue]
decodeSeqIntOrStr "" xs  = return $ reverse xs
decodeSeqIntOrStr x xs =
  case decodeIntOrStr x of
    Just (BInt p) ->
      decodeSeqIntOrStr (drop (length $ encodeInt p) x) (BInt p : xs)
    Just (BStr p) ->
      decodeSeqIntOrStr (drop (length $ encodeStr p) x) (BStr p : xs)
    _ -> Nothing
    
decodeLst :: String -> Maybe [BValue]
decodeLst "" = Nothing
decodeLst (x:xs)
  | x /= 'l'= Nothing
  | otherwise = decodeSeqIntOrStr (take (length xs - 1) xs) []
