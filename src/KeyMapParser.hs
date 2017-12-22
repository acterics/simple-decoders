module KeyMapParser(getKeyMap) where

type KeyMap = [(String, String)]

getKeyMap :: IO ()
getKeyMap = readFile "res/key-map.data" >>= putStrLn