module RotorMachine where


import Data.List.Split
import Data.Char 
import Lib

type Rotor = [Int]

getRotors :: IO [Rotor]
getRotors = readFile "res/rotor-machine/rotors.data" >>=
    return . (map $ (map digitToInt) . filter (/=' ')) . (splitOn "\n")

getRotorMachineAlphatet :: IO Alphabet
getRotorMachineAlphatet = readFile "res/rotor-machine/alphabet.data" >>=
    return . (zip [0..]) . (map $ ord . head) . (splitOn " ")


decodeRotorMachine :: String -> IO String
decodeRotorMachine encoded = return ""


encodeRotorMachine :: String -> IO String
encodeRotorMachine input = return ""