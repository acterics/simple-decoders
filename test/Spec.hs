
import UU.PPrint
import ViginereDecoder
import CesarDecoder
import RotorMachine


main :: IO ()
main = do 
    -- showDecodedViginere 
    -- showDecodedCesar
    getRotorMachineAlphatet >>= putStrLn . show
    getRotors >>= putStrLn . show
    where 
    showDecodedViginere = decodeViginere >>= putDocList . (map pretty)
    showDecodedCesar = decodeCesar >>= putDocList . (map pretty)
    
    putDocList (x:xs) = putDoc x >> putStrLn "" >> putDocList xs
    putDocList _ = putStrLn "###################"
    