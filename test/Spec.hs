
import UU.PPrint
import ViginereDecoder
import CesarDecoder


main :: IO ()
main = do 
    showDecodedViginere 
    showDecodedCesar
    where 
    showDecodedViginere = decodeViginere >>= putDocList . (map pretty)
    showDecodedCesar = decodeCesar >>= putDocList . (map pretty)
    
    putDocList (x:xs) = putDoc x >> putStrLn "" >> putDocList xs
    putDocList _ = putStrLn "###################"
    