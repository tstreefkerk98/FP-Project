module Jq.Filters where

import Jq.Json (JSON)
data Filter = Identity
  | Identifier String
  | OptIdentifier String
  | Slice (Int, Int)
  | OptSlice (Int, Int)
  | Iterator
  | OptIterator
  | IteratorJArray [Int]
  | OptIteratorJArray [Int]
  | IteratorJObject [String]
  | OptIteratorJObject [String]
  | CommaOperator Filter Filter
  | PipeOperator Filter Filter
  | ValueCons JSON
  | ValueConsArray [Filter]
  | ValueConsObject [(Filter, Filter)]
  | Group Filter
  | TryCatch Filter Filter
  | EmptyCatch

instance Show Filter where
  show Identity                = "."
  show (Identifier i)          = show i
  show (OptIdentifier i)       = show i
  show (Slice (l, r))          = show l ++ ":" ++ show r
  show (OptSlice (l, r))       = show l ++ ":" ++ show r
  show Iterator                = show ".[]"
  show OptIterator             = show ".[]"
  show (IteratorJArray is)     = show ".[" ++ show is ++ "]"
  show (OptIteratorJArray is)  = show ".[" ++ show is ++ "]"
  show (IteratorJObject is)    = show ".[" ++ show is ++ "]"
  show (OptIteratorJObject is) = show ".[" ++ show is ++ "]"
  show (CommaOperator l r)     = show l ++ show r
  show (PipeOperator l r)      = show l ++ show r
  show (ValueCons j)           = show j
  show (ValueConsArray fs)     = show fs
  show (ValueConsObject fs)    = show fs
  show (Group f)               = show f
  show (TryCatch f s)          = show f ++ show s
  show EmptyCatch              = show "Empty catch"

data Config = ConfigC {filters :: Filter}
