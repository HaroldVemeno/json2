{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module CLI where

import System.Environment ( getArgs )
import System.CPUTime ( getCPUTime )
import Data.Char ( toLower )
import Data.Foldable ( Foldable(toList) )
import Data.Bifunctor ( Bifunctor(bimap) )

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as E
import qualified Data.Scientific as SC
import qualified Data.HashMap.Lazy as M

import qualified Types as Json
import qualified Parse as P
import qualified MParse as MP
import qualified MParseText as MPT
import qualified Data.Aeson as Aeson
import Show



parseDefault = parseWith ["mparset"] 

defMain :: IO ()
defMain = do
    putStrLn ""
    args <- getArgs
    if null args then 
        error "No action"
    else let (action:rest) = args in case map toLower action of 
        "parsewith" -> parseWith rest
        "parse" -> parseDefault rest
        "time" -> time rest
        "timeall" -> timeAll rest
        _ -> error (action ++ " is not an action")
    

parseWith args = if null args then error "Missing parser name" else 
                 let (parser:rest) = args in do
    case map toLower parser of 
        "parse" -> undefined
        "mparse" -> undefined
        "mparsetext" -> undefined
        "mparset"    -> undefined
        "aeson" -> undefined
        _ -> error (parser ++ " is not a parser name")

time args = if null args then error "Missing parser name" else 
            let (parser:rest) = args in 
    case map toLower parser of 
        "parse" -> timeParse rest
        "mparse" -> timeMParse rest
        "mparsetext" -> timeMParseT rest
        "mparset"    -> timeMParseT rest
        "aeson" -> timeAeson rest
        _ -> error (parser ++ " is not a parser name")

timeParser :: String  -> -- Name
             (a -> b) -> -- The parser
              a       -> -- The data
              IO ()

aesonParse :: BS.ByteString -> Either String Json.Json
aesonParse bs = toJson <$> Aeson.eitherDecodeStrict bs
    where 
        toJson :: Aeson.Value -> Json.Json
        toJson Aeson.Null = Json.Null
        toJson (Aeson.Bool b) = Json.Bool b
        toJson (Aeson.Number n) = Json.Number . SC.toRealFloat $ n
        toJson (Aeson.String t) = Json.String . T.unpack $ t
        toJson (Aeson.Array a) = Json.Array . toList $ toJson <$> a
        toJson (Aeson.Object o) = 
                Json.Object . M.fromList . fmap (bimap T.unpack toJson) . M.toList $ o

timeAll args = do
    filebs <- if null args then BS.getContents else BS.readFile (head args)
    let !filet = E.decodeUtf8 filebs
    let !files = BS.unpack filebs
    timeParser "Parse" P.parseJson files
    timeParser "MParse"  MP.parseJson files
    timeParser "MParseText" MPT.parseJson filet
    timeParser "Aeson" aesonParse filebs 
    putStrLn "That's all"

testShowJson args = do
    filet <- if null args then T.getContents else T.readFile (head args)
    timePS "jsonParse" MPT.parseJson showJson filet
    
testPrettyShowJson args = do
    filet <- if null args then T.getContents else T.readFile (head args)
    timePS "jsonParse" MPT.parseJson prettyShowJson filet

timePS name parse serialize file = do
    let !json = parse file
    case json of 
        Left a -> do
            putStr "Error:"
            print a
        Right a -> do
            putStrLn $ name ++ " Start!"
            start <- getCPUTime 
            let !s = serialize a
            end <- getCPUTime 
            putStrLn $ name ++ " End!"
            let t = fromIntegral $ end - start
            putStrLn $ "Took " ++ show t ++ "s"

timeParser name parse file = do
    putStrLn $ name ++ " Start!"
    start <- getCPUTime 
    let !json = parse file
    end <- getCPUTime 
    putStrLn $ name ++ " End!"
    let t = (/(10^12)) . fromIntegral $ end - start 
    putStrLn $ "Took " ++ show t ++ "s of cpu time"

timeParse args = do 
    file <- if null args then getContents else readFile (head args)
    timeParser "Parse" P.parseJson file
    
timeMParse args = do 
    file <- if null args then getContents else readFile (head args)
    timeParser "MParse" MP.parseJson file
timeMParseT args = do 
    file <- if null args then T.getContents else T.readFile (head args)
    timeParser "MParseText" MPT.parseJson file
timeAeson args = do
    file <- if null args then BS.getContents else BS.readFile (head args)
    timeParser "Aeson" aesonParse file
    
