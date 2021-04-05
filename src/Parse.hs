module Parse (parseJson) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Char ( ord, chr )
import qualified Data.HashMap.Lazy as M
import Control.Monad ( void )

import Types ( Json )
import qualified Types as J

ws :: Parser ()
ws = void . many $ oneOf " \n\t\r"

parseJson :: String -> Either String Json
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
    ) <|> noneOf "\""
    where toul = chr . foldl1 (\a b -> 16 * a + b)
          hexd = (\x -> ord x - ord '0') <$> oneOf "0123456789"
             <|> (\x -> ord x - ord 'a' + 10) <$> oneOf "abcdef"
             <|> (\x -> ord x - ord 'A' + 10) <$> oneOf "ABCDEF"

object :: Parser Json
object = J.Object . M.fromList <$ char '{' <* ws <*> sepEndBy (kvp <* ws) (char ',' *> ws) <* char '}'
  where kvp = (,) <$> str <* ws <* char ':' <* ws <*> json

array :: Parser Json 
array = J.Array <$ char '[' <* ws <*> sepEndBy (json <* ws) (char ',' <* ws) <* char ']'

jnum :: Parser Json
jnum = J.Number <$> (makedbl
   <$> ( True <$ char '-' <|> pure False) 
   <*> (    [0] <$  char  '0' 
        <|> (:) <$> (dtoint <$> oneOf "123456789") <*> many digit)
   <*> ( char '.' *> many digit 
        <|> pure [] )
   <*> ( (,) <$ oneOf "eE" <*> (True <$ char '-' <|> False <$ optional (char '+')) <*> many digit
        <|> pure (False, []) )
   )
    where 
        digit = dtoint <$> oneOf "0123456789"
        dtoint a = ord a - ord '0'

makedbl :: Bool -> [Int] -> [Int] -> (Bool, [Int]) -> Double
makedbl neg darr fracarr (nege, exparr) = 
     let  neg' = (if neg then -1 else 1) 
          mant = ( foldl (\a b -> 10*a + fromIntegral b) 0 darr 
            + foldr (\a b -> b/10 + fromIntegral a) 0 fracarr / 10)
          exp = (10**(foldl (\a b -> 10*a + fromIntegral b) 0 exparr 
            * if nege then -1 else 1))
     in neg' * mant * exp
                                            
