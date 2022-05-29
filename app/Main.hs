module Main where
import GHC.IO.Encoding
import Adventure 

main :: IO ()
main = do 
    setLocaleEncoding utf8
    gameMain 
