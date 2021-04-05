{-# LANGUAGE BangPatterns #-}
module Show (showJson, prettyShowJson) where

import Data.Monoid
import Data.List
import Control.Monad
import qualified Data.Char as C
import qualified Data.HashMap.Lazy as M

import Types ( Json )
import qualified Types as J

showJson, prettyShowJson :: Json -> String
prettyShowJson = unlines . sj
  where
    sj :: Json -> [String]
    sj J.Null = ["null"]
    sj (J.Bool b) = [if b then "true" else "false"]
    sj (J.Number n) = [show n]
    sj (J.String s) = [ "\"" ++ (s >>= escape) ++ "\"" ]
    sj (J.Array []) = ["[]"]
    sj (J.Array a) = let elems = map sj a in
                       if length (foldr1 (++) $ foldr1 (++) elems) < 40 then
                           ["[ " ++ intercalate ", " (map (\[a] -> a) elems) ++ " ]" ]
                       else 
                           join [ ["["], 
                                  join $ 
                                      map (\e -> map ("  " ++) $ notlast e <: (last e ++ ",")) (notlast elems) 
                                      <: map ("  " ++) (last elems),
                                  ["]"]
                                ]

    sj (J.Object o) | M.null o = ["{}"]
    sj (J.Object o) = let mp = M.toList o
                          elems = map (\(k,v) -> ("\"" ++ k ++ "\": " ++ head (sj v) ) : tail (sj v) ) mp in
                        if length (foldr1 (++) $ foldr1 (++) elems) < 40 then
                           ["{ " ++ intercalate ", " (map (\[a] -> a) elems) ++ " }" ]
                        else 
                           join [ ["{"], 
                                  join $ 
                                      map (\e -> map ("  " ++) $ notlast e <: (last e ++ ",")) (notlast elems) 
                                      <: map ("  " ++) (last elems),
                                  ["}"]
                                ]
    notlast :: [a] -> [a]
    notlast [] = []
    notlast [a] = []
    notlast (a:b) = a : notlast b

    infixl 5 <:
    (<:) :: [a] -> a -> [a]
    [] <: a = [a]
    [a] <: b = a:[b]
    (a:b) <: c = a:(b<:c)

escape :: Char -> String
escape c = case c of 
      '\\' -> "\\\\"
      '"'  -> "\\\""
      '\n' -> "\\n"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _    -> [c] 

showJson J.Null = "null"
showJson (J.Bool b) = if b then "true" else "false"
showJson (J.Number n) = show n
showJson (J.String s) =  "\"" ++ (s >>= escape) ++ "\"" 
showJson (J.Array a) = '[' : foldl' (\s x -> if null s then showJson x else s ++ "," ++ showJson x) "" a ++ "]"
showJson (J.Object o) = let a = M.toList o in 
    '{' : foldl' (\s (k, v) -> if null s then '"' : k ++ "\":" ++ showJson v else s ++ ",\"" ++ k ++ "\":" ++ showJson v) "" a ++ "}"
