
module Jq.Json where

data JSON = JString String
    | JNumber Double
    | JBool Bool
    | JNull
    | JObject [(String, JSON)]
    | JArray [JSON]

instance Show JSON where
  show n = showJSON 0 [n]

showJSON :: Int -> [JSON] -> String
showJSON _ []           = []
showJSON _ [JString n]  = "\"" ++ n ++ "\""
showJSON _ [JNumber n]  = if n - fromIntegral r == 0 then show r else show n
  where r = round n
showJSON _ [JBool n]    = if n then "true" else "false"
showJSON _ [JNull]      = "null"
showJSON _ [JObject []] = "{}" 
showJSON i [JObject ns] = "{\n" ++ showObject (i + 1) ns ++ "\n" ++ getIndent i ++ "}"
  where
    showObject _ []       = []
    showObject j [(k, v)] = getIndent j ++ "\"" ++ k ++ "\"" ++ ": " ++ showJSON j [v]
    showObject j (x:xs)   = showObject j [x] ++ ",\n" ++ showObject j xs
showJSON _ [JArray []]  = "[]"
showJSON i [JArray ns]  = "[\n" ++ showArray (i + 1) ns ++ "\n" ++ getIndent i ++ "]"
  where
    showArray _ []     = []
    showArray j [x]    = getIndent j ++ showJSON j [x]
    showArray j (x:xs) = showArray j [x] ++ ",\n" ++ showArray j xs 
showJSON _ _ = "Error during printing" 

getIndent :: Int -> String
getIndent d = concat (replicate d "  ")
