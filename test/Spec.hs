module Main where

import CLI ( timeAll )
import System.Environment
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  file <- fromMaybe "test/test.json" <$> lookupEnv "TEST_FILE" 
  timeAll [file]
