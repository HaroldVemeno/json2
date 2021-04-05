{-# LANGUAGE OverloadedStrings #-}
module MParseText where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import Data.Char ( ord, chr )
import qualified Data.HashMap.Lazy as M
import Control.Monad ( void )
import Data.Void (Void)

import Types ( Json ) 
import qualified Types as J

type Parser = Parsec Void T.Text

ws :: Parser ()
ws = void . many $ oneOf (" \n\t\r" :: String)

parseJson :: T.Text -> Either String Json
parseJson str = case parse start "" str of 
     Right a -> Right a
     Left e -> Left $ show e

start :: Parser Json
start = ws *> json <* ws <* eof

json :: Parser Json
json = array <|> object <|> jstr <|> jnum <|> bool <|> null
    where
        bool = J.Bool True  <$ string "true"
           <|> J.Bool False <$ string "false"
        null = J.Null <$ string "null"
        jstr = J.String <$> str

str :: Parser String
str = char '"' *> many codepoint <* char '"'

codepoint :: Parser Char
codepoint = char '\\' *> (
        '\\' <$ char '\\'
    <|> '"'  <$ char '"'
    <|> '\b' <$ char 'b'
    <|> '/'  <$ char '/'
    <|> '\f' <$ char 'f'
    <|> '\n' <$ char 'n'
    <|> '\r' <$ char 'r'
    <|> '\t' <$ char 't'
    <|> toul <$ char 'u' <*> count 4 hexd
    ) <|> satisfy (/= '"')
    where toul = chr . foldl1 (\a b -> 16 * a + b)
          hexd = (\x -> ord x - ord '0') <$> digitChar 
             <|> (\x -> ord x - ord 'a' + 10) <$> oneOf ['a'..'f']
             <|> (\x -> ord x - ord 'A' + 10) <$> oneOf ['A'..'F']

object :: Parser Json
object = J.Object . M.fromList <$ char '{' <* ws <*> sepEndBy (kvp <* ws) (char ',' *> ws) <* char '}'
  where kvp = (,) <$> str <* ws <* char ':' <* ws <*> json

array :: Parser Json 
array = J.Array <$ char '[' <* ws <*> sepEndBy (json <* ws) (char ',' <* ws) <* char ']'

jnum :: Parser Json
jnum = J.Number <$> (makedbl
   <$> ( True <$ char '-' <|> pure False) 
   <*> (    [0] <$  char  '0' 
        <|> (:) <$> (dtoint <$> oneOf ['1'..'9']) <*> many digit)
   <*> ( char '.' *> many digit 
        <|> pure [] )
   <*> ( (,) <$ oneOf ['e','E'] <*> (True <$ char '-' <|> False <$ optional (char '+')) <*> many digit
        <|> pure (False, []) )
   )
    where 
        digit = dtoint <$> digitChar
        dtoint a = ord a - ord '0'

makedbl :: Bool -> [Int] -> [Int] -> (Bool, [Int]) -> Double
makedbl neg darr fracarr (nege, exparr) = 
     let  neg' = (if neg then -1 else 1) 
          mant = ( foldl (\a b -> 10*a + fromIntegral b) 0 darr 
            + foldr (\a b -> b/10 + fromIntegral a) 0 fracarr / 10)
          exp = (10**(foldl (\a b -> 10*a + fromIntegral b) 0 exparr 
            * if nege then -1 else 1))
     in neg' * mant * exp
                                            
