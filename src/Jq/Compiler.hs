-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Compiler where

import           Jq.Filters
import           Jq.Json

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity             inp = return [inp]
-- compile (Identifier i)       obj = compileIdentifier i obj False
-- compile (OptIdentifier i)    obj = compileIdentifier i obj True
-- compile (Index i)            obj = compileIndex i obj False
-- compile (OptIndex i)         obj = compileIndex i obj True
compile (Slice (l, r))       obj = compileSlice (l, r) obj False
compile (OptSlice (l, r))    obj = compileSlice (l, r) obj True
compile Iterator             obj = compileIterator obj False
compile OptIterator          obj = compileIterator obj True
compile (IteratorJArray is)  obj = compileIteratorJArray is obj False
compile (OptIteratorJArray is)  obj = compileIteratorJArray is obj True
compile (IteratorJObject is) obj    = compileIteratorJObject is obj False
compile (OptIteratorJObject is) obj = compileIteratorJObject is obj True
-- compile _ _ = return []

compileIdentifier :: String -> JSON -> Bool -> Either String [JSON]
compileIdentifier i (JObject obj) _ = if not (null f) then Right [snd (head f)] else Right [JNull]
    where f = filter ((==i).fst) obj
compileIdentifier _ _         True  = Right []
compileIdentifier _ obj       False = Left ("Cannot apply identifier to " ++ getJqType obj)

-- compileIndex :: Int -> JSON -> Bool -> Either String [JSON]
-- compileIndex i (JArray ns) _
--     | i < size && -i <= size = Right [ns !! getIndex size i]
--     | otherwise              = Right [JNull]
--     where size = length ns
-- compileIndex _ _   True      = Right []
-- compileIndex _ obj False     = Left ("Cannot apply index to " ++ getJqType obj)

compileSlice :: (Int, Int) -> JSON -> Bool -> Either String [JSON]
compileSlice (l, r) (JArray ns) _ = Right [JArray (take (r' - l') (drop l' ns))]
    where 
        size = length ns
        r' = getIndex size r
        l' = getIndex size l
compileSlice (l, r) (JString s) _ = Right [JString (take (r' - l') (drop l' s))]
    where 
        size = length s
        r' = getIndex size r
        l' = getIndex size l
compileSlice _      _       True  = Right []
compileSlice _      obj     False = Left ("Cannot apply slice to " ++ getJqType obj)

compileIterator :: JSON -> Bool -> Either String [JSON]
compileIterator (JArray ns)   opt = compileIteratorJArray [] (JArray ns) opt
compileIterator (JObject obj) opt = compileIteratorJObject [] (JObject obj) opt
compileIterator _           True  = Right []
compileIterator obj         False = Left ("Cannot apply iterator to " ++ getJqType obj)

compileIteratorJArray :: [Int] -> JSON -> Bool -> Either String [JSON]
compileIteratorJArray [] (JArray ns) _ = Right (foldr (:) [] ns)
compileIteratorJArray is (JArray ns) _ = Right (foldr (:) [] [ getValue i | i <- is] )
    where 
        size        = length ns
        getValue i' = if i' < size && -i' <= size then ns !! getIndex size i' else JNull
compileIteratorJArray _  _       True  = Right []
compileIteratorJArray _  obj     False = Left ("Cannot apply JArray iterator to " ++ getJqType obj)

compileIteratorJObject :: [String] -> JSON -> Bool -> Either String [JSON]
compileIteratorJObject is (JObject obj) _ = Right [ snd o | o <- obj , fst o `elem` is ]
compileIteratorJObject _  _         True  = Right []
compileIteratorJObject _  obj       False = Left ("Cannot apply JObject iterator to " ++ getJqType obj)

getIndex :: Int -> Int -> Int
getIndex size i = if i < 0 then size + i else i

getJqType :: JSON -> String
getJqType (JString _) = "JString"
getJqType (JNumber _) = "JNumber"
getJqType (JBool _)   = "JBool"
getJqType JNull       = "JNull"
getJqType (JArray _)  = "JArray"
getJqType (JObject _) = "JObject"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
