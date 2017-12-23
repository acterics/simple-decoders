module CesarDecoder(decodeCesar) where

import Lib
import Data.Char

decodeCesar :: IO [String]
decodeCesar = getEncoded "res/cesar/encoded.data" >>= 
    \encoded -> getKeyMap "res/cesar/key-map.data" >>=
        \keyMap -> decodeAll encoded keyMap


decodeAll :: String -> KeyMap -> IO [String]            
decodeAll encoded keyMap = return $ map showIndex $ zip indexes $ map (denormalize . decodeWithKey) indexes where
    decodeWithKey key = map ((decodeShifted key) .ord ) encoded
    decodeShifted key charcter  = chr (getKeyMapValue (mod ((getKeyMapPosition charcter keyMap) - key) (length keyMap)) keyMap)
    indexes = [0..((length keyMap) - 1)]
    showIndex (index, value) = foldl (++) "" [
        (show index), ": ",
        [(chr $ getKeyMapValue index keyMap)], ": ", 
        (value)
        ]
