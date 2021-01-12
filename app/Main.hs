module Main where

import System.IO
import Control.Monad (unless)
import Parser

-- ref. https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2

main :: IO ()
-- main = do interact doread
main = do
  putStr "LambdaCalc> "
  hFlush stdout
  exp <- getLine
  unless (exp == ":q")
    $ putStrLn (show $ parse exp)
    >> main
    
    
