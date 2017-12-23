module Lib where

import Data.List.Split
import Data.String.Utils
import Data.Char 

type Alphabet = [(Int, Int)]

getEncoded :: String -> IO String
getEncoded encodedPath = readFile encodedPath >>= return .normalizeComa . normalizeHyphen

getAlphabet :: String -> IO Alphabet
getAlphabet alphabetPath = readFile alphabetPath >>= return . splitFile . normalizeAlphabetComa . normalizeAlphabetHyphen

splitFile :: String -> Alphabet
splitFile file = filter filterError $ map (toCortege . splitOn "-") $ splitOn "," file where 
    toCortege [_, []] = (-1, 0)
    toCortege [code, value] = (read (lstrip code), ord $ head value)
    toCortege _ = (-1, 0)
    filterError (code, _) = code /= -1


findEntry :: ((Int, Int) -> Bool) -> [(Int, Int)] -> (Int, Int)
findEntry predicate (x:xs) 
        | predicate x = x
        | otherwise = findEntry (predicate) xs
findEntry _ _ = (-1, -1)

getAlphabetValue :: Int -> Alphabet -> Int
getAlphabetValue position = snd . findEntry (\(first, _) -> first == position)

getAlphabetPosition :: Int -> Alphabet -> Int
getAlphabetPosition value = fst . findEntry (\(_, second) -> second == value)


normalizeAlphabetComa :: String -> String
normalizeAlphabetComa = replace ",," "#,"

normalizeComa :: String -> String
normalizeComa = replace "," "#"

normalizeHyphen :: String -> String
normalizeHyphen = replace "-" "$"

normalizeAlphabetHyphen :: String -> String
normalizeAlphabetHyphen = replace "--" "-$"

denormalizeComa :: String -> String
denormalizeComa = replace "#" ","

denormalizeHyphen :: String -> String
denormalizeHyphen = replace "$" "-"

denormalize :: String -> String
denormalize = (replace "#" ",") . (replace "$" "-")