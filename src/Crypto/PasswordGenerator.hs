module Crypto.PasswordGenerator
( Flag(..)
, genPassword
) where

import Control.Applicative
import Data.List
import Test.QuickCheck.Gen

data Flag = Digits
          | Uppers
          | Lowers
          | Symbols
          | Ambiguous
          | Vowels
            deriving (Eq)

-- | Generate a password of required length with provided flags.
genPassword :: [Flag] -> Int -> IO String
genPassword flags len = try
  where
    try = do
      l <- sample' $ vectorOf len $ oneof gens
      let v = find good l
      case v of
        Just x -> return x
        Nothing -> try
    gens = map toGen flags
    good = foldl' (\a v -> (liftA2 (&&)) a $ toChecker v) alwaysTrue flags

digitList    = sort "0123456789"
upperList    = sort "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lowerList    = sort "abcdefghijklmnopqrstuvwxyz"
symbolList   = sort "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
ambiguouList = sort "B8G6I1l0OQDS5Z2"
vowelList    = sort "01aeiouyAEIOUY"

toGen Digits    = elements digitList
toGen Uppers    = elements upperList
toGen Lowers    = elements lowerList
toGen Symbols   = elements symbolList
toGen Ambiguous = elements ambiguouList
toGen Vowels    = elements vowelList

toChecker Digits  s = s `containsAnyOf` digitList
toChecker Uppers  s = s `containsAnyOf` upperList
toChecker Symbols s = s `containsAnyOf` symbolList
toChecker _       s = True

alwaysTrue s = True

s `containsAnyOf` l = not $ null $ intersect l s
