{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

class PolyInt r where
  retInt :: Int -> r

instance PolyInt Int where
  retInt = id

instance (a ~ Int, PolyInt r) => PolyInt (a -> r) where
  retInt x y = retInt (x + y)

polyAdd :: (PolyInt a) => a
polyAdd = retInt 0

class PolyList a r | r -> a where
  retList :: [a] -> r

instance PolyList a [a] where
  retList = id

instance PolyList a r => PolyList a (a -> r) where
  retList xs x = retList (xs ++ [x])

polyList :: PolyList a r => r
polyList = retList []

class PolyWord r where
  retWord :: String -> r

instance PolyWord String where
  retWord w = case w of
    [] -> ""
    _ -> tail w

instance (a ~ String, PolyWord r) => PolyWord (a -> r) where
  retWord x y = retWord (x ++ " " ++ y)

polyWords :: PolyWord r => r
polyWords = retWord ""

main :: IO ()
main = print (polyList 1 2 3 :: [Int])
