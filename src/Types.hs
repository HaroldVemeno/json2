module Types where

import Data.HashMap.Lazy ( HashMap ) 

data Json 
  = String String
  | Number Double
  | Bool Bool
  | Null
  | Object (HashMap String Json)
  | Array [Json]
    deriving (Read, Show)

