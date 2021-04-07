{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module MParseBS where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B (w2c)
import Data.Char (chr, ord)
import qualified Data.HashMap.Lazy as M
import Data.Void (Void)
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte
import Types (Json)
import qualified Types as J

type Parser = Parsec Void B.ByteString
#define DQw8 34
#define BSw8 92
#define CMAw8 44
#define DSHw8 45
#define DOTw8 46
#define CLNw8 58
#define OBCw8 123
#define CBCw8 125
#define OBKw8 91
#define CBKw8 93

ws :: Parser ()
ws = void . takeWhileP Nothing $ (`elem` [9,10,13,32]) -- many . anyOf " \n\t\r"

parseJson :: B.ByteString -> Either String Json
parseJson str = case parse start "" str of
  Right a -> Right a
  Left e -> Left $ errorBundlePretty e

start :: Parser Json
start = ws *> json <* ws <* eof

json :: Parser Json
json = array <|> object <|> jstr <|> jnum <|> bool <|> null
  where
    bool =
      J.Bool True <$ string "true"
        <|> J.Bool False <$ string "false"
    null = J.Null <$ string "null"
    jstr = J.String <$> str

str :: Parser String
str = char DQw8 *> rest
    where
      rest = do 
        cps <- B.unpack <$> takeWhileP Nothing (\x -> x /= BSw8 && x /= DQw8) 
        (cps <$ char DQw8) 
         <|> ((\x y -> cps++[x]++y) <$ char BSw8 <*> escaped <*> rest)

      escaped = '\\' <$ char BSw8
            <|> '"'  <$ char DQw8
            <|> '\b' <$ char 98  --'b'
            <|> '/'  <$ char 47  --'\'
            <|> '\f' <$ char 102 --'f'
            <|> '\n' <$ char 110 --'n'
            <|> '\r' <$ char 114 --'r'
            <|> '\t' <$ char 116 --'t'
            <|> toul <$ char 117 {-'u'-} <*> count 4 hexd

      toul = chr . foldl1 (\a b -> 16 * a + b)
      hexd =  (\x -> fromEnum x - 48 {-0-}) <$> digitChar
        <|> (\x -> fromEnum x - 87 {- (- 'a' + 10) -}) <$> satisfy (\x -> 97 <= x && x <= 102)
                                                                -- oneOf [97..102] -- ['a'..'f']
        <|> (\x -> fromEnum x - 55 {- (- 'A' + 10) -}) <$> satisfy (\x -> 65 <= x && x <= 70)
                                                                -- oneOf [65..70]  -- ['A'..'F']

object :: Parser Json
object = J.Object . M.fromList <$ char OBCw8 <* ws <*> sepEndBy (kvp <* ws) (char CMAw8 *> ws) <* char CBCw8
  where
    kvp = (,) <$> str <* ws <* char CLNw8 <* ws <*> json

array :: Parser Json
array = J.Array <$ char OBKw8 <* ws <*> sepEndBy (json <* ws) (char CMAw8 <* ws) <* char CBKw8

jnum :: Parser Json
jnum =
  J.Number
    <$> ( makedbl
            <$> (True <$ char DSHw8 <|> pure False)
            <*> ( [0] <$ char 48 --'0'
                    <|> (:) <$> (dtoint <$> satisfy (\x -> 49 <= x && x <= 57 {-'1'..'9'-}))
                      <*> many digit
                )
            <*> ( char DOTw8 *> many digit
                    <|> pure []
                )
            <*> ( (,) <$ oneOf [69, 101 {- "eE" -}]
                    <*> (True <$ char DSHw8 <|> False <$ optional (char 43 {-'+'-}))
                    <*> many digit
                    <|> pure (False, [])
                )
        )
  where
    digit = dtoint <$> digitChar
    dtoint a = fromEnum a - 48 -- '0'

makedbl :: Bool -> [Int] -> [Int] -> (Bool, [Int]) -> Double
makedbl neg darr fracarr (nege, exparr) =
  let neg' = (if neg then -1 else 1)
      mant =
        ( foldl (\a b -> 10 * a + fromIntegral b) 0 darr
          + foldr (\a b -> b / 10 + fromIntegral a) 0 fracarr / 10
        )
      exp =
        ( 10 ** ( foldl (\a b -> 10 * a + fromIntegral b) 0 exparr
                  * if nege then -1 else 1
                )
        )
   in neg' * mant * exp
