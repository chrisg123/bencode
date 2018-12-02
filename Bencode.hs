 module Bencode where
 bfind :: String -> String
 bfind s =
   let (i, len) = bfindR s 0
    in take len (drop i s)

 bfindR :: String -> Int -> (Int, Int)
 bfindR "" i = (max 0 (i - 1), 0)
 bfindR (x:xs) i
   | x `elem` ['0' .. '9'] =
     let metaString = takeWhile (`elem` ['0' .. '9']) (x : xs)
         metaLength = length metaString
         dataLength = read metaString :: Int
         colonFollows = (x : xs) !! metaLength == ':'
      in if colonFollows
           then (i, metaLength + 1 + dataLength)
           else bfindR xs (i + 1)
   | otherwise = bfindR xs (i + 1)
