module Bencode where
import Data.Char (isDigit)
import Data.Map.Strict as M

data BValue
  = BStr String
  | BInt Int
  | BLst [BValue]
  | BDct (Map String BValue)
  deriving (Eq, Show, Ord)

encodeInt :: Int -> String
encodeInt x = "i" ++ show x ++ "e"

decodeInt :: String -> Maybe Int
decodeInt (x0:x1:x2:xs)
  | x0 /= 'i' ||
      x1 /= '-' && not (isDigit x1) ||
      x1 == '-' && not (isDigit x2) || last (x2 : xs) /= 'e' = Nothing
  | otherwise =
    let d = x1 : takeWhile isDigit (x2 : xs)
     in if length d == length (x1 : x2 : xs) - 1
          then return (read d :: Int)
          else Nothing
decodeInt _ = Nothing

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

decodeLstOrDct :: String -> Maybe BValue
decodeLstOrDct x =
  case decodeLst x of
    Just y -> return $ BLst y
    _ -> decodeDct x >>= \y -> return $ BDct y

decodeAny :: String -> Maybe BValue
decodeAny x =
  case decodeIntOrStr x of
    Just y -> return y
    _ -> decodeLstOrDct x >>= \y -> return y

decodeSeq :: String -> [BValue] -> Maybe [BValue]
decodeSeq "" xs  = return $ reverse xs
decodeSeq x xs =
  case decodeAny x of
    Just (BInt p) ->
      decodeSeq (drop (length $ encodeInt p) x) (BInt p : xs)
    Just (BStr p) ->
      decodeSeq (drop (length $ encodeStr p) x) (BStr p : xs)
    Just (BLst p) -> decodeSeq (drop (length $ encodeLst p) x) (BLst p : xs)
    Just (BDct p) -> decodeSeq (drop (length $ encodeDct p) x) (BDct p : xs)
    _ -> Nothing
    
decodeLst :: String -> Maybe [BValue]
decodeLst "" = Nothing
decodeLst (x:xs)
  | x /= 'l' || last xs /= 'e' = Nothing
  | otherwise = decodeSeq (take (length xs - 1) xs) []

encodeLst :: [BValue] -> String
encodeLst bs = 'l' : encodeCol (Left bs) "" ++ "e"

decodeDct :: String -> Maybe (Map String BValue)
decodeDct "" = Nothing
decodeDct (x:xs)
  | x /= 'd' || xs == "" || last xs /= 'e' = Nothing
  | otherwise =
    decodeSeq (take (length xs - 1) xs) [] >>= \y ->
      let f (BStr b0:b1:bs) = (b0, b1) : f bs
          f (BInt b0:b1:bs) = (show b0, b1) : f bs
          f _ = []
          keyValPairs = f y
          complete = 2 * length keyValPairs == length y
       in if complete
            then Just $ M.fromList keyValPairs
            else Nothing

encodeDct ::  Map String BValue -> String
encodeDct d = 'd' : encodeCol (Right (toList d)) "" ++ "e"

encodeCol :: Either [BValue] [(String, BValue)] -> String -> String
encodeCol (Left []) s = s
encodeCol (Right []) s = s
encodeCol x s =
  let (key, val, xs) =
        either
          (\(w:ws) -> ("", w, Left ws))
          (\((p, q):ws) -> (show (length p) ++ ":" ++ p, q, Right ws))
          x
   in case val of
        BInt b -> encodeCol xs (s ++ key ++ encodeInt b)
        BStr b -> encodeCol xs (s ++ key ++ encodeStr b)
        BLst b -> encodeCol xs (s ++ key ++ encodeLst b)
        BDct b -> encodeCol xs (s ++ key ++ encodeDct b)
