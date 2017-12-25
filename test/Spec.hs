
import UU.PPrint
import ViginereDecoder
import CesarDecoder
import RotorMachine


main :: IO ()
main = do 
    showDecodedViginere 
    showDecodedCesar
    showDebugEncodedRotorMachine
    showEncodedRotorMachine
    showDecodedRotorMachine
    showDebugDecodedRotorMachine
    where 
    showDecodedViginere = decodeViginere >>= putDocList . (map pretty)
    showDecodedCesar = decodeCesar >>= putDocList . (map pretty)
    showDebugEncodedRotorMachine = putStrLn "" >> debugEncodeRotorMachine >>= putDocList . (map pretty)
    showEncodedRotorMachine = putStr "Encoded: " >> encodeRotorMachine 27 >>=  putDoc . pretty
    showDecodedRotorMachine = decodeRotorMachine >>= putDocList . (map pretty)
    showDebugDecodedRotorMachine = putStrLn "" >> debugDecodeRotorMachine 1 >>= putDocList . (map pretty)


    putDocList (x:xs) = putDoc x >> putStrLn "" >> putDocList xs
    putDocList _ = putStrLn "###################"
    