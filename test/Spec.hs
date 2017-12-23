
import Lib
import UU.PPrint

main :: IO ()
main = decodeViginere >>= \decoded -> putDocList $ map (pretty) decoded where
    putDocList (x:xs) = putDoc x >> putStrLn "" >> putDocList xs
    putDocList _ = putStrLn "###################"