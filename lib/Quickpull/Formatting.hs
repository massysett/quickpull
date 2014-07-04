module Quickpull.Formatting where

indentAmt :: Int
indentAmt = 2

indent :: Int -> String -> String
indent d s = replicate (d * 2) ' ' ++ s ++ "\n"

(<+>) :: String -> String -> String
l <+> r
  | null l || null r = l ++ r
  | otherwise = l ++ " " ++ r

titles :: [String] -> String
titles = concat . zipWith indent [0..] . reverse
