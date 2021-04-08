-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Compiler where

import           Jq.Filters
import           Jq.Json

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity                obj = return [obj]
compile (Identifier i)          obj = compileIdentifier i obj False 
compile (OptIdentifier i)       obj = compileIdentifier i obj True
compile (Slice (l, r))          obj = compileSlice (l, r) obj False
compile (OptSlice (l, r))       obj = compileSlice (l, r) obj True
compile Iterator                obj = compileIterator obj False
compile OptIterator             obj = compileIterator obj True
compile (IteratorJArray is)     obj = compileIteratorJArray is obj False
compile (OptIteratorJArray is)  obj = compileIteratorJArray is obj True
compile (IteratorJObject is)    obj = compileIteratorJObject is obj False
compile (OptIteratorJObject is) obj = compileIteratorJObject is obj True
compile (CommaOperator l r)     obj = compileCommaOperator (CommaOperator l r) obj
compile (PipeOperator l r)      obj = compilePipeOperator (PipeOperator l r) [obj]
compile (ValueCons json)        _   = compileValueCons json
compile (ValueConsArray fs)     obj = compileValueConsArray fs obj
compile (ValueConsObject fs)    obj = compileValueConsObject fs obj

compileIdentifier :: String -> JSON -> Bool -> Either String [JSON]
compileIdentifier i (JObject obj) _ = if not (null f) then Right [snd (head f)] else Right [JNull]
    where f = filter ((==i).fst) obj
compileIdentifier _ _         True  = Right [JNull]
compileIdentifier _ obj       False = Left ("Cannot apply identifier to " ++ getJqType obj)

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
compileSlice _      JNull   _     = Right [JNull]        
compileSlice _      _       True  = Right []
compileSlice _      obj     False = Left ("Cannot apply slice to " ++ getJqType obj)

compileIterator :: JSON -> Bool -> Either String [JSON]
compileIterator (JArray ns)   opt = compileIteratorJArray [] (JArray ns) opt
compileIterator (JObject obj) opt = compileIteratorJObject [] (JObject obj) opt
compileIterator _           True  = Right []
compileIterator obj         False = Left ("Cannot apply iterator to " ++ getJqType obj)

compileIteratorJArray :: [Int] -> JSON -> Bool -> Either String [JSON]
compileIteratorJArray [] (JArray ns) _ = Right ns
compileIteratorJArray is (JArray ns) _ = Right [ getValue i | i <- is ]
    where 
        size        = length ns
        getValue i' = if i' < size && -i' <= size then ns !! getIndex size i' else JNull
compileIteratorJArray is  _       True  = Right (replicate (length is) JNull)
compileIteratorJArray _  obj      False = Left ("Cannot apply JArray iterator to " ++ getJqType obj)

compileIteratorJObject :: [String] -> JSON -> Bool -> Either String [JSON]
compileIteratorJObject [] (JObject obj) _ = Right (map snd obj)
compileIteratorJObject is (JObject obj) _ = Right [ snd o | o <- obj , fst o `elem` is ]
compileIteratorJObject _  _         True  = Right [JNull]
compileIteratorJObject _  obj       False = Left ("Cannot apply JObject iterator to " ++ getJqType obj)

compileCommaOperator :: Filter -> JSON -> Either String [JSON]
compileCommaOperator (CommaOperator l r) obj = 
    do
        l' <- compile l obj
        r' <- compileCommaOperator r obj
        return (l' ++ r')
compileCommaOperator f                   obj = compile f obj 

compilePipeOperator :: Filter -> [JSON] -> Either String [JSON]
compilePipeOperator _                  []     = Right []
compilePipeOperator (PipeOperator l r) (o:os) = 
    do
        l' <- compile l o
        r' <- compilePipeOperator r l'
        ts <- compilePipeOperator (PipeOperator l r) os
        return (r' ++ ts)
compilePipeOperator f                  (o:os) = 
    do
        x <- compile f o
        xs <- compilePipeOperator f os
        return (x ++ xs)

compileValueCons :: JSON -> Either String [JSON]
compileValueCons json = Right [json]

compileValueConsArray :: [Filter] -> JSON -> Either String [JSON]
compileValueConsArray fs obj = 
    do 
        is <- mapM (`compile` obj) fs
        return [JArray (concat is)]

compileValueConsObject :: [(String, Filter)] -> JSON -> Either String [JSON]
compileValueConsObject fs obj = 
    do
        let ss = map fst fs
        js <- mapM ((`compile` obj) . snd) fs
        let xs = zip ss (concat js)
        return [JObject xs]



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
