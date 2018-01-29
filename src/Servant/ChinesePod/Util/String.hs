-- | Utility functions on strings
module Servant.ChinesePod.Util.String (
    explode
  , dropBOM
  , trim
  , lowercase
  ) where

import Data.Char

-- | Split a string at the specified delimeter
explode :: Char -> String -> [String]
explode needle = go
  where
    go :: String -> [String]
    go haystack = case break (== needle) haystack of
                   (xs, "")                 -> [xs]
                   (xs, _needle':haystack') -> xs : go haystack'

-- | Drop unicode BOM character if present
dropBOM :: String -> String
dropBOM ('\65279':str) = str
dropBOM str            = str

-- | Trim whitespace
trim :: String -> String
trim = rtrim . ltrim
  where
    ltrim, rtrim :: String -> String
    ltrim = dropWhile isSpace
    rtrim = reverse . ltrim . reverse

-- | Map every character to lowercase
lowercase :: String -> String
lowercase = map toLower
