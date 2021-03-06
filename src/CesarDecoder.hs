module CesarDecoder(decodeCesar) where

import Lib
import Data.Char

decodeCesar :: IO [String]
decodeCesar = getEncoded "res/cesar/encoded.data" >>= 
    \encoded -> getAlphabet "res/cesar/alphabet.data" >>=
        \alphabet -> decodeAll encoded alphabet


decodeAll :: String -> Alphabet -> IO [String]            
decodeAll encoded alphabet = return $ map showIndex $ zip indexes $ map (denormalize . decodeWithKey) indexes where
    decodeWithKey key = map ((decodeShifted key) .ord ) encoded
    decodeShifted key charcter  = chr (valueOf (mod ((indexOf charcter alphabet) - key) (length alphabet)) alphabet)
    indexes = [0..((length alphabet) - 1)]
    showIndex (index, value) = foldl (++) "" [
        (show index), ": ",
        [(chr $ valueOf index alphabet)], ": ", 
        (value)
        ]

        