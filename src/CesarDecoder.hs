module CesarDecoder(decodeCesar) where

import Lib
import Data.Char
import UU.PPrint

decodeCesar :: IO [String]
decodeCesar = getEncoded "res/cesar/encoded.data" >>= 
    \encoded -> getKeyMap "res/cesar/key-map.data" >>=
        \keyMap -> decodeAll encoded keyMap


decodeAll :: String -> KeyMap -> IO [String]            
decodeAll encoded keyMap = do
    -- logKeyMap
    -- return [] where
    return $ map showIndex $ zip indexes $ map (denormalize . decodeWithKey) indexes where
    decodeWithKey key = map ((decodeShifted key) .ord ) encoded
    -- decodeShifted char key = chr (getKeyMapValue (mod ((getKeyMapPosition char keyMap) - key) (length keyMap)) keyMap)
    decodeShifted key char  = chr (getKeyMapValue (mod ((getKeyMapPosition char keyMap) - key) (length keyMap)) keyMap)
    indexes = [0..((length keyMap) - 1)]
    showIndex (index, value) = (show index) ++ ": " ++ [(chr $ getKeyMapValue index keyMap)] ++ ": " ++ (value)


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
