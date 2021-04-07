module Jq.JParser where

import Parsing.Parsing
import Jq.Json

import Prelude

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJBool :: Parser JSON
parseJBool = 
    do
        _ <- string "true"
        return (JBool True)
    <|>
    do
        _ <- string "false"
        return (JBool False)

parseJString :: Parser JSON 
parseJString = 
    do
        _ <- symbol "\""
        s <- many (alphanum <|> char ' ' <|> (char '\\' *> char '"') <|> char '\\')
        _ <- symbol "\""
        return (JString s)

parseJNumber :: Parser JSON
parseJNumber = 
    do 
        l <- integer
        _ <- symbol "."
        r <- integer
        e <- symbol "E-" <|> symbol "E+" <|> symbol "E"
        p <- integer
        let n = (read(show l ++ "." ++ show r) :: Double) * (if e == "E+" || e == "E" then 10 else 1/10)^p
        return (JNumber n)
    <|>
    do
        l <- integer
        _ <- symbol "."
        r <- integer
        let n = read(show l ++ "." ++ show r) :: Double
        return (JNumber n)
    <|>
    do 
        i <- integer
        e <- symbol "E-" <|> symbol "E+" <|> symbol "E"
        p <- integer
        let n = (read(show i) :: Double) * (if e == "E+" || e == "E" then 10 else 1/10)^p
        return (JNumber n)
    <|>
    do
        n <- integer
        return (JNumber (fromIntegral n))

parseJArray :: Parser JSON 
parseJArray = 
    do
        _ <- symbol "["
        es <- many (parseJSON <* symbol ",")
        if not (null es) then
            do
                le <- parseJSON
                _  <- symbol "]"
                return (JArray (es ++ [le]))
        else
            do
                e <- many parseJSON
                _ <- symbol "]"
                if not (null e) then
                    return (JArray e)
                else
                    return (JArray [])

parseJObject :: Parser JSON 
parseJObject = 
    do
        _ <- symbol "{"
        _ <- symbol "}"
        return (JObject [])
    <|>
    do
        _ <- symbol "{"
        p <- many $
            do 
                k <- symbol "\"" *> identifier <* symbol "\""
                _ <- symbol ":"
                v <- parseJSON
                _ <- symbol ","
                return (k, v)
        l <- some $ 
            do
                k <- symbol "\"" *> identifier <* symbol "\""
                _ <- symbol ":"
                v <- parseJSON
                return (k, v)
        _ <- symbol "}"
        return (JObject (p ++ l))

parseJSON :: Parser JSON
parseJSON = token $ parseJArray
    <|> parseJObject
    <|> parseSingleJSON

parseSingleJSON :: Parser JSON 
parseSingleJSON = token $ parseJNull
    <|> parseJString
    <|> parseJNumber
    <|> parseJBool
