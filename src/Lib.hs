module Lib(decodeViginere) where

import Data.List.Split
import Data.Char 
import Data.String.Utils
import qualified Codec.Binary.UTF8.String as UTF
import UU.PPrint

type Key = [Char]
type Keys = [Key]
type KeyMap = [(Int, Int)]


getEncoded :: IO String
getEncoded = readFile "res/encoded.data" >>= return .normalizeComa . normalizeHyphen

getKeys :: IO Keys
getKeys = readFile "res/keys.data" >>= return . splitOn "\n" . normalizeComa . normalizeHyphen


getKeyMap :: IO KeyMap
getKeyMap = readFile "res/key-map.data" >>= return . splitFile . normalizeKeyMapComa . normalizeKeyMapHyphen

splitFile :: String -> KeyMap
splitFile file = filter filterError $ map (toCortege . splitOn "-") $ splitOn "," file where 
    toCortege [_, []] = (-1, 0)
    toCortege [code, value] = (read (lstrip code), ord $ head value)
    toCortege _ = (-1, 0)
    filterError (code, _) = code /= -1


decodeViginere :: IO [String]
decodeViginere = getEncoded >>= 
    \encoded -> getKeys >>= 
        \keys -> getKeyMap >>= 
            \keyMap -> decodeAll encoded keys keyMap where 
                

decodeAll :: [Char] -> Keys -> KeyMap -> IO [String]
decodeAll encoded keys keyMap = 
    return $ decodeWithDenormalizing keys where
    decodeWithKey key = map (chr . fromKeyMap) (zip (map ord encoded) (cycle $ map ord key))
    fromKeyMap (encode, key) = getKeyMapValue (mod ((getKeyMapPosition encode) - (getKeyMapPosition key)) (length keyMap))
    getKeyMapValue position = snd (findEntry (\(first, _) -> first == position) keyMap)
    getKeyMapPosition value = fst (findEntry (\(_, second) -> second == value) keyMap)
    decodeWithDenormalizing = map (denormalizeComa . denormalizeHyphen . decodeWithKey)


    -- Utils for ligging
    logKeyMap = logMap $ map mapKeyEntry keyMap
    logMap (x:xs) = logMapEntry x >> logMap xs
    logMap [] = putStrLn "#########"
    logMapEntry (position, char) = putStr ("(" ++ show position ++ ": ") >> putDoc char >> putStrLn ")"

    logKeys (x:xs) = logKey x >> logKeys xs
    logKeys [] = putStrLn "#######"
    logKey key = putDoc (pretty key) >> putStrLn ""

    logCodes (x:xs) = putStrLn x >> logCodes xs
    logCodes [] = putStrLn "#########"

    mapKeyEntry (position, code) = (position, (pretty [chr code]))
    
findEntry :: ((Int, Int) -> Bool) -> [(Int, Int)] -> (Int, Int)
findEntry predicate (x:xs) 
        | predicate x = x
        | otherwise = findEntry (predicate) xs
findEntry _ _ = (-1, -1)


normalizeKeyMapComa :: String -> String
normalizeKeyMapComa = replace ",," "#,"

normalizeComa :: String -> String
normalizeComa = replace "," "#"

normalizeHyphen :: String -> String
normalizeHyphen = replace "-" "$"

normalizeKeyMapHyphen :: String -> String
normalizeKeyMapHyphen = replace "--" "-$"

denormalizeComa :: String -> String
denormalizeComa = replace "#" ","

denormalizeHyphen :: String -> String
denormalizeHyphen = replace "$" "-"
