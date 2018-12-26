module Bencode where
import Data.Char (isDigit)
import Data.Map.Strict as M

data BValue
  = BStr String
  | BInt Int
  | BLst [BValue]
  | BDct (Map String BValue)
  deriving (Eq, Show, Ord)

encode :: BValue -> String
encode x =
  case x of
    BInt b -> "i" ++ show b ++ "e"
    BStr b -> show (length b) ++ ":" ++ b
    BLst b -> 'l' : encodeCol (Left b) "" ++ "e"
    BDct b -> 'd' : encodeCol (Right (toList b)) "" ++ "e"

decode :: String -> Maybe BValue
decode x = g 0
  where
    f 0 = decodeInt x >>= \w -> return $ BInt w
    f 1 = decodeStr x >>= \w -> return $ BStr w
    f 2 = decodeLst x >>= \w -> return $ BLst w
    f 3 = decodeDct x >>= \w -> return $ BDct w
    f _ = Nothing
    g :: Int -> Maybe BValue
    g n =
      case f n of
        Just y -> return y
        Nothing -> g (n + 1)

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

decodeSeq :: String -> [BValue] -> Maybe [BValue]
decodeSeq "" bs  = return $ reverse bs
decodeSeq x bs =
  let f = decode x >>= \y -> return (y, encode y)
      g (_, "") = Nothing
      g (decoded, encoded) = decodeSeq (drop (length encoded) x) (decoded : bs)
   in f >>= g

decodeLst :: String -> Maybe [BValue]
decodeLst "" = Nothing
decodeLst (x:xs)
  | x /= 'l' || last xs /= 'e' = Nothing
  | otherwise = decodeSeq (take (length xs - 1) xs) []

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
       in if 2 * length keyValPairs == length y
            then Just $ M.fromList keyValPairs
            else Nothing

-- |Encode a collection of BValues. (i.e. Either list or dictionary)
encodeCol :: Either [BValue] [(String, BValue)] -> String -> String
encodeCol (Left []) s = s
encodeCol (Right []) s = s
encodeCol x s =
  let (key, val, xs) =
        either
          (\(w:ws) -> ("", w, Left ws))
          (\((p, q):ws) -> (show (length p) ++ ":" ++ p, q, Right ws))
          x
   in encodeCol xs (s ++ key ++ encode val)
