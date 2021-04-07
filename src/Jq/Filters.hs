module Jq.Filters where

data Filter = Identity
  -- | Identifier String
  -- | OptIdentifier String
  -- | Index Int
  -- | OptIndex Int
  | Slice (Int, Int)
  | OptSlice (Int, Int)
  | Iterator
  | OptIterator
  | IteratorJArray [Int]
  | OptIteratorJArray [Int]
  | IteratorJObject [String]
  | OptIteratorJObject [String]

instance Show Filter where
  show Identity                = "."
  -- show (Identifier i)          = show i
  -- show (OptIdentifier i)       = show i
  -- show (Index i)               = show i
  -- show (OptIndex i)            = show i
  show (Slice (l, r))          = show l ++ ":" ++ show r
  show (OptSlice (l, r))       = show l ++ ":" ++ show r
  show Iterator                = show ".[]"
  show OptIterator             = show ".[]"
  show (IteratorJArray is)     = show ".[" ++ show is ++ "]"
  show (OptIteratorJArray is)  = show ".[" ++ show is ++ "]"
  show (IteratorJObject is)    = show ".[" ++ show is ++ "]"
  show (OptIteratorJObject is) = show ".[" ++ show is ++ "]"

data Config = ConfigC {filters :: Filter}
