module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser
import Jq.Json

parseSlice :: Parser Filter 
parseSlice = 
  do
    _ <- token . char $ '.'
    _ <- symbol "["
    l <- integer
    _ <- symbol ":"
    r <- integer
    _ <- symbol "]"
    q <- many (symbol "?")
    if null q then return (Slice (l, r)) else return (OptSlice (l, r))

parseIterator :: Parser Filter
parseIterator = 
  do
    il <- do
          _ <- token . symbol $ "."
          _ <- symbol "[\""
          i <- identifier
          _ <- symbol "\"]"
          return i
    q <- many (symbol "?")
    if null q then return (Identifier il) else return (OptIdentifier il)
  <|>
  do
    is <- some (
      do
        _ <- token . symbol $ "."
        s <- symbol "\"" <|> symbol ""
        i <- identifier
        _ <- (if s == "\"" then symbol "\"" else symbol "")
        return i
      )
    q <- many (symbol "?")
    if length is == 1 then
      if null q then return (Identifier (head is)) else return (OptIdentifier (head is))
    else
      return (getPipeIdentifier is)
  <|>
  do
    _ <- token . symbol $ "."
    _ <- symbol "[]"
    q <- many (symbol "?")
    if null q then return Iterator else return OptIterator
  <|>
  do
    _ <- token . symbol $ "."
    _ <- symbol "["
    is <- many (integer <* symbol ",")
    t <- integer
    _ <- symbol "]"
    q <- many (symbol "?")
    if null q then return (IteratorJArray (is ++ [t])) else return (OptIteratorJArray (is ++ [t]))
  <|>
  do
    _ <- token . char $ '.'
    _ <- symbol "["
    is <- many(symbol "\"" *> identifier <* symbol "\",")
    t <- symbol "\"" *> identifier <* symbol "\""
    _ <- symbol "]"
    q <- many (symbol "?")
    if null q then return (IteratorJObject (is ++ [t])) else return (OptIteratorJObject (is ++ [t]))

getPipeIdentifier :: [String] -> Filter 
getPipeIdentifier []     = Identity 
getPipeIdentifier [i]    = Identifier i
getPipeIdentifier (i:is) = PipeOperator (Identifier i) (getPipeIdentifier is)

parseExtraFilters :: Parser Filter
parseExtraFilters = 
  do
    _ <-symbol "[]"
    return Iterator 

getPipeIterator :: [String] -> Filter 
getPipeIterator []      = Identity 
getPipeIterator (_:is)  = PipeOperator Iterator (getPipeIterator is)

parsePipe :: Parser Filter 
parsePipe = 
  do
    l <- token $ parseSingleFilter
    r <- symbol "|" *> parseFilter
    return (PipeOperator l r)

parseComma :: Parser Filter
parseComma = 
  do
    l <- token $ parseSingleFilter
    r <- symbol "," *> parseFilter
    return (CommaOperator l r)

parseIdentity :: Parser Filter
parseIdentity = 
  do
    _ <- token . symbol $ "."
    return Identity

parseValueConsArray :: Parser Filter 
parseValueConsArray = 
  do
    _ <- token . symbol $ "["
    is <- many (parseFilter <* symbol ",")
    l <- parseFilter
    _ <- symbol "]"
    return (ValueConsArray (is ++ [l]))
  <|>
  do
    _ <- token . symbol $ "[]"
    return (ValueConsArray [])

parseValueConsObject :: Parser Filter 
parseValueConsObject = 
  do
    _ <- token . symbol $ "{}"
    return (ValueConsObject [])
  <|>
  do
    _ <- token . symbol $ "{"
    ts <- some (
      do
        g <- 
          do parseGroup <* symbol ":"
          <|>
          do
            g' <- (parseString <|> identifier) <* symbol ":"
            return (ValueCons (JString g'))
        f <- parseFilter
        _ <- many (symbol ",")
        return (g, f)
      <|>
      do
        g <- parseString <|> identifier
        _ <- many (symbol ",")
        return (ValueCons (JString g), Identifier g)
      )
    _ <- symbol "}"
    return (ValueConsObject ts)

parseGroup :: Parser Filter 
parseGroup = 
  do
    _ <- token . symbol $ "("
    g <- parseFilter
    _ <- symbol ")"
    return (Group g)

parseFilter :: Parser Filter
parseFilter = parsePipe
  <|> parseComma
  <|> parseValueConsArray
  <|> parseValueConsObject
  <|> do ValueCons <$> parseSingleJSON
  <|> parseSingleFilter 

parseSingleFilter :: Parser Filter
parseSingleFilter = parseSlice
  <|> do 
        fs <- some (parseIterator <|> parseExtraFilters)
        return (createPipe fs)
  <|> parseIdentity
  <|> parseGroup

createPipe :: [Filter] -> Filter
createPipe []     = Identity 
createPipe (f:fs) = PipeOperator f (createPipe fs)

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
