module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

-- parseIdentifier :: Parser Filter 
-- parseIdentifier = 
--   do
--     _ <- token . char $ '.'
--     _ <- string "[\""
--     i <- identifier
--     _ <- string "\"]"
--     q <- many (char '?')
--     if null q then return (Identifier i) else return (OptIdentifier i)
--   <|>
--   do
--     _ <- token . char $ '.'
--     i <- identifier
--     q <- many (char '?')
--     if null q then return (Identifier i) else return (OptIdentifier i)

-- parseIndex :: Parser Filter 
-- parseIndex =
--   do
--     _ <- token . char $ '.'
--     _ <- char '['
--     i <- integer
--     _ <- char ']'
--     q <- many (char '?')
--     if null q then return (Index i) else return (OptIndex i)

parseSlice :: Parser Filter 
parseSlice = 
  do
    _ <- token . char $ '.'
    _ <- char '['
    l <- integer
    _ <- symbol ":"
    r <- integer
    _ <- char ']'
    q <- many (char '?')
    if null q then return (Slice (l, r)) else return (OptSlice (l, r))

parseIterator :: Parser Filter 
parseIterator = 
  do
    _ <- token . char $ '.'
    _ <- string "[]"
    q <- many (char '?')
    if null q then return Iterator else return OptIterator
  <|>
  do
    _ <- token . char $ '.'
    _ <- char '['
    is <- many (integer <* char ',')
    t <- integer
    _ <- char ']'
    q <- many (char '?')
    if null q then return (IteratorJArray (is ++ [t])) else return (OptIteratorJArray (is ++ [t]))
  <|>
  do
    _ <- token . char $ '.'
    _ <- char '['
    is <- many(symbol "\"" *> identifier <* symbol "\",")
    t <- symbol "\"" *> identifier <* symbol "\""
    _ <- char ']'
    q <- many (char '?')
    if null q then return (IteratorJObject (is ++ [t])) else return (OptIteratorJObject (is ++ [t]))

parseIdentity :: Parser Filter
parseIdentity = 
  do
    _ <- token . char $ '.'
    return Identity

parseFilter :: Parser Filter
-- parseFilter = parseIdentifier
  -- <|> parseIndex
parseFilter = parseSlice
  -- <|> parseSlice
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
