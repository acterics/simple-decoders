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
        \keys -> getKeyMap "res/viginere/key-map.data" >>= 
            \keyMap -> decodeAll encoded keys keyMap where 
                

decodeAll :: [Char] -> Keys -> KeyMap -> IO [String]
decodeAll encoded keys keyMap = 
    return $ decodeWithDenormalizing keys where
    decodeWithKey key = map (chr . fromKeyMap) (zip (map ord encoded) (cycle $ map ord key))
    fromKeyMap (encode, key) = getKeyMapValue (mod ((getKeyMapPosition encode keyMap) - (getKeyMapPosition key keyMap)) (length keyMap)) keyMap
    decodeWithDenormalizing = map (denormalize . decodeWithKey)


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
    
