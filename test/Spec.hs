module Main where

import CLI ( timeAll )

main :: IO ()
main = timeAll ["test/test.json"]
