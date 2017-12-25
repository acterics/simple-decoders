module RotorMachine where


import Data.List.Split
import Data.Char 
import Lib

type Rotor = [(Int, Int)]

getRotors :: IO [Rotor]
getRotors = readFile "res/rotor-machine/rotors.data" >>=
    return . (map $ ((zip [0..]) . map (read) . splitOn " ")) . (splitOn "\n")

getRotorMachineAlphabet :: IO Alphabet
getRotorMachineAlphabet = readFile "res/rotor-machine/alphabet.data" >>=
    return . (zip [0..]) . (map $ ord . head) . (splitOn " ")




decodeRotorMachine ::  IO [String]
decodeRotorMachine = readFile "res/rotor-machine/encoded.data" >>=
    \encoded -> getRotorMachineAlphabet >>=
        \alphabet -> getRotors >>=
            \rotors -> decodeAll encoded alphabet rotors 
 

decodeAll :: String -> Alphabet -> [Rotor] -> IO [String]
decodeAll encoded alphabet rotors = return $ map decodeWithOffset alphabet where
    decodeWithOffset (index, _) = (show index) ++ " " ++ (decode encoded alphabet rotors index)


decode :: String -> Alphabet -> [Rotor] -> Int -> String
decode input alphabet rotors offset = (decodeChar (map ord input) offset) where
    decodeChar (code:inputTail) letterOffset = 
        (rotorsDecode (reverse rotors) code letterOffset 0) : (decodeChar inputTail $ mod (letterOffset + 1) (length alphabet))
    decodeChar _ _ = ""
    rotorsDecode (rotor:rotorsTail) code letterOffset rotorIndex = 
        rotorsDecode rotorsTail (s $ c $ p letterOffset) letterOffset (rotorIndex + 1)  where
        p rotorOffset
            | rotorIndex == 0 = (indexOf code alphabet) + 1
            | otherwise = indexOf (valueOf (indexOf code alphabet)  (shiftAlphabet alphabet rotorOffset)) alphabet
        c index = (valueOf (mod (index - 1) (length alphabet)) rotor)
        s index = valueOf (mod (index - 1) (length alphabet)) alphabet
    rotorsDecode [] code _ _ = chr code


encodeRotorMachine :: Int -> IO String
encodeRotorMachine offset = readFile "res/rotor-machine/input.data" >>=
    \input -> getRotorMachineAlphabet >>=
        \alphabet -> getRotors >>=
            \rotors -> encode input alphabet rotors offset

encode :: String -> Alphabet -> [Rotor] -> Int -> IO String
encode input alphabet rotors offset = return (encodeChar (map ord input) offset) where
    encodeChar (code:inputTail) letterOffset = 
        (rotorsEncode rotors code letterOffset 0) : (encodeChar inputTail $ mod (letterOffset + 1) (length alphabet))
    encodeChar _ _ = ""
    rotorsEncode (rotor:rotorsTail) code letterOffset rotorIndex = 
        rotorsEncode rotorsTail (s $ p $ c letterOffset) letterOffset (rotorIndex + 1)  where
        c rotorOffset  
            | rotorIndex == 0 = mod ((indexOf code alphabet) + 1) (length alphabet)
            | otherwise = indexOf code (shiftAlphabet alphabet rotorOffset)
        p index = mod (indexOf index rotor + 1) (length alphabet)
        s index = valueOf (mod (index - 1) (length alphabet)) alphabet
    rotorsEncode [] code _ _ = chr code


    
debugDecodeRotorMachine :: Int -> IO [String]
debugDecodeRotorMachine offset = readFile "res/rotor-machine/encoded.data" >>=
    \encoded -> getRotorMachineAlphabet >>=
        \alphabet -> getRotors >>=
            \rotors -> debugDecodeAll encoded alphabet rotors offset


debugDecodeAll :: String -> Alphabet -> [Rotor] -> Int -> IO [String]
debugDecodeAll encoded alphabet rotors offset = 
    return $ [headers] ++  (decodeChar (map (ord) encoded) offset) where
    decodeChar (code:inputTail) letterOffset = [getStepDebugInfo code letterOffset] ++ (decodeChar inputTail $ mod (letterOffset + 1) (length alphabet)) 
    decodeChar _ _ = []
    debugDecodeByRotor (rotor:rotorsTail) code letterOffset rotorIndex =
        (map getFieldWithSpaces fields) ++ (debugDecodeByRotor rotorsTail (s $ c $ p letterOffset) letterOffset (rotorIndex + 1))
        where
        fields = [ 
            [chr code],
            show $ c $ p letterOffset,
            show $ p letterOffset
            ]  
        p rotorOffset
            | rotorIndex == 0 = (indexOf code alphabet) + 1
            | otherwise = indexOf (valueOf (indexOf code alphabet)  (shiftAlphabet alphabet rotorOffset)) alphabet
        c index = (valueOf (mod (index - 1) (length alphabet)) rotor)
        s index = valueOf (mod (index - 1) (length alphabet)) alphabet
    debugDecodeByRotor [] code _ _ = [getFieldWithSpaces [chr code]]
    getStepDebugInfo code letterOffset = (getFieldWithSpaces $ show letterOffset) ++ (foldl (++) "" $ reverse (debugDecodeByRotor (reverse rotors) code letterOffset 0))
    
    headers = (getFieldWithSpaces "зсув") ++ (rotorsFields rotors 0)
    rotorsFields (_:rotorsTail) index = foldl (++) "" (map getFieldWithSpaces [
        "s" ++ show index,
        "c" ++ show index,
        "p" ++ show index
        ]) ++ rotorsFields rotorsTail (index + 1)
    rotorsFields [] index = getFieldWithSpaces ('s': (show index))

debugEncodeRotorMachine :: IO [String]
debugEncodeRotorMachine = readFile "res/rotor-machine/input.data" >>=
    \input -> getRotorMachineAlphabet >>=
        \alphabet -> getRotors >>=
            \rotors -> debugEncodeAll input alphabet rotors 27     


debugEncodeAll :: String -> Alphabet -> [Rotor] -> Int -> IO [String]
debugEncodeAll input alphabet rotors offset = 
    return $ [headers] ++  (encodeChar (map (ord) input) offset) where
    encodeChar (code:inputTail) letterOffset = [getStepDebugInfo code letterOffset] ++ (encodeChar inputTail $ mod (letterOffset + 1) (length alphabet)) 
    encodeChar _ _ = []
    debugEncodeByRotor (rotor:rotorsTail) code letterOffset rotorIndex =
        (foldl (++) "" (map getFieldWithSpaces fields)) ++ debugEncodeByRotor rotorsTail (s $ p $ c letterOffset) letterOffset (rotorIndex + 1) 
        where
        fields = [ 
            [chr code],
            show $ c letterOffset,
            show $ p $ c letterOffset
            ]  
        c rotorOffset  
            | rotorIndex == 0 = (indexOf code alphabet) + 1
            | otherwise = indexOf code (shiftAlphabet alphabet rotorOffset)
        p index = (indexOf index rotor + 1)
        s index = valueOf (mod (index - 1) (length alphabet)) alphabet  
    debugEncodeByRotor [] code _ _ = [chr code]
    getStepDebugInfo code letterOffset = (getFieldWithSpaces $ show letterOffset) ++ (debugEncodeByRotor rotors code letterOffset 0)
    
    headers = (getFieldWithSpaces "зсув") ++ (rotorsFields rotors 1)
    rotorsFields (_:rotorsTail) index = foldl (++) "" (map getFieldWithSpaces [
        "s" ++ show index,
        "c" ++ show index,
        "p" ++ show index
        ]) ++ rotorsFields rotorsTail (index + 1)
    rotorsFields [] index = getFieldWithSpaces ('s': (show index))

    

shiftAlphabet :: Alphabet -> Int -> Alphabet
shiftAlphabet alphabet shiftOffset = map (shift shiftOffset) alphabet where
    shift offset (index, value) = (mod (index + offset) (length alphabet), value)