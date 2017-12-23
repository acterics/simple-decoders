module ViginereDecoder(decodeViginere) where

import Lib
import Data.List.Split
import Data.Char 
import UU.PPrint

type Key = [Char]
type Keys = [Key]

getKeys :: IO Keys
getKeys = readFile "res/viginere/keys.data" >>= return . splitOn "\n" . normalizeComa . normalizeHyphen


decodeViginere :: IO [String]
decodeViginere = getEncoded "res/viginere/encoded.data" >>= 
    \encoded -> getKeys >>= 
        \keys -> getAlphabet "res/viginere/key-map.data" >>= 
            \alphabet -> decodeAll encoded keys alphabet where 
                

decodeAll :: [Char] -> Keys -> Alphabet -> IO [String]
decodeAll encoded keys alphabet = 
    return $ decodeWithDenormalizing keys where
    decodeWithKey key = map (chr . fromalphabet) (zip (map ord encoded) (cycle $ map ord key))
    fromalphabet (encode, key) = getAlphabetValue (mod ((getAlphabetPosition encode alphabet) - (getAlphabetPosition key alphabet)) (length alphabet)) alphabet
    decodeWithDenormalizing = map (denormalize . decodeWithKey)


    -- Utils for ligging
    logalphabet = logMap $ map mapKeyEntry alphabet
    logMap (x:xs) = logMapEntry x >> logMap xs
    logMap [] = putStrLn "#########"
    logMapEntry (position, char) = putStr ("(" ++ show position ++ ": ") >> putDoc char >> putStrLn ")"

    logKeys (x:xs) = logKey x >> logKeys xs
    logKeys [] = putStrLn "#######"
    logKey key = putDoc (pretty key) >> putStrLn ""

    logCodes (x:xs) = putStrLn x >> logCodes xs
    logCodes [] = putStrLn "#########"

    mapKeyEntry (position, code) = (position, (pretty [chr code]))
    
