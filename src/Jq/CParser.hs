module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser

-- parseIdentifier :: Parser Filter 
-- parseIdentifier =
  -- do 
  --   is <- some (
  --     do
  --       _ <- token . symbol $ "."
  --       s <- symbol "[\"" <|> symbol "\"" <|> symbol ""
  --       i <- identifier
  --       _ <- (if s == "[\"" then symbol "\"]" else if s == "\"" then symbol "\"" else symbol "")
  --       return i
  --     )
  --   q <- many (symbol "?")
  --   if length is == 1 then
  --     if null q then return (Identifier (head is)) else return (OptIdentifier (head is))
  --   else
  --     return (getPipe is)
  -- <|>
  -- do
  --   _ <- token . symbol $ "."
  --   i <- identifier
  --   is <- many (symbol "." *> identifier)
  --   q <- many (symbol "?")
  --   if null is then
  --     if null q then return (Identifier i) else return (OptIdentifier i)
  --   else
  --     return (getPipe (i:is))

getPipe :: [String] -> Filter 
getPipe []     = Identity 
getPipe [i]    = Identifier i
getPipe (i:is) = PipeOperator (Identifier i) (getPipe is)

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
    -- ts <- many (symbol "[]")
    q <- many (symbol "?")
    -- if null ts then
    if null q then return (Identifier il) else return (OptIdentifier il)
    -- else
      -- if length ts == 1 then
        -- return (PipeOperator (Identifier il) Iterator)
      -- else
        -- return (PipeOperator (Identifier il) (PipeOperator Iterator Iterator))
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
      return (getPipe is)
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

parseComma :: Parser Filter
parseComma = 
  do
    l <- token $ parseSingleFilter
    r <- symbol "," *> parseFilter
    return (CommaOperator l r)

parsePipe :: Parser Filter 
parsePipe = 
  do
    l <- token $ parseSingleFilter
    r <- symbol "|" *> parseFilter
    return (PipeOperator l r)

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

parseValueConsObject :: Parser Filter 
parseValueConsObject = 
  do
    _ <- token . symbol $ "{"
    a <- many (identifier <* symbol ":")
    b <- many parseFilter
    _ <- symbol "}"
    return (ValueConsObject (zip a b))

parseFilter :: Parser Filter
parseFilter = do ValueCons <$> parseSingleJSON
  <|> parseValueConsArray
  <|> parseValueConsObject
  <|> parsePipe
  <|> parseComma
  <|> parseSingleFilter

parseSingleFilter :: Parser Filter
parseSingleFilter = parseSlice
  <|> parseIterator
  <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
