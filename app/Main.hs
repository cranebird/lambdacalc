module Main where

import System.IO
import Control.Monad (unless, liftM)
import Parser
import LambdaTerm

main :: IO ()
main = do
  putStr "LambdaCalc> "
  hFlush stdout
  exp <- getLine
  unless (exp == ":q") $ do
    putStrLn (show $ parse exp)
    putStrLn (showTerm $ parse exp)
    main
