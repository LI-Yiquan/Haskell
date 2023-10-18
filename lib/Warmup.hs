{-
     Do not change the skeleton code! The point of this
     assignment is to figure out how the functions can
     be written this way (using fold). You may only
     replace the `error "TBD:..."` parts.

     For this assignment, you may use the following library functions:

     map
     foldl'
     foldr
     length
     append (or ++)
     zip
 -}

module Warmup where

import Prelude  hiding (replicate, sum, reverse)
import Data.List (foldl')

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft = foldl'

foldRight :: (b -> a -> a) -> a -> [b] -> a
foldRight = foldr


-- | Sum the elements of a list
--
-- >>> sumList [1, 2, 3, 4]
-- 10
--
-- >>> sumList [1, -2, 3, 5]
-- 7
--
-- >>> sumList [1, 3, 5, 7, 9, 11]
-- 36

sumList :: [Int] -> Int
--sumList xs = error "TBD:sumList"
sumList []     = 0
sumList (x:xs) = x + sumList xs    


-- | `digitsOfInt n` should return `[]` if `n` is not positive,
--    and otherwise returns the list of digits of `n` in the
--    order in which they appear in `n`.
--
-- >>> digitsOfInt 3124
-- [3, 1, 2, 4]
--
-- >>> digitsOfInt 352663
-- [3, 5, 2, 6, 6, 3]

digitsOfInt :: Int -> [Int]
digitsOfInt 0 = []
-- digitsOfInt n = error "TBD:digitsOfInt" 
digitsOfInt n
  | n <= 0    = []                                  
  | otherwise = digitsOfInt (n `div` 10) ++ [n `mod` 10]  


-- | `digits n` retruns the list of digits of `n`
--
-- >>> digits 31243
-- [3,1,2,4,3]
--
-- digits (-23422)
-- [2, 3, 4, 2, 2]

digits :: Int -> [Int]
digits n = digitsOfInt (abs n)


-- | From http://mathworld.wolfram.com/AdditivePersistence.html
--   Consider the process of taking a number, adding its digits,
--   then adding the digits of the number derived from it, etc.,
--   until the remaining number has only one digit.
--   The number of additions required to obtain a single digit
--   from a number n is called the additive persistence of n,
--   and the digit obtained is called the digital root of n.
--   For example, the sequence obtained from the starting number
--   9876 is (9876, 30, 3), so 9876 has
--   an additive persistence of 2 and
--   a digital root of 3.
--
-- NOTE: assume additivePersistence & digitalRoot are only called with positive numbers

-- >>> additivePersistence 9876
-- 2

additivePersistence :: Int -> Int
-- additivePersistence n = error "TBD"
additivePersistence n = persistenceHelper n 0
  where
    persistenceHelper n count
      | n < 10    = count
      | otherwise = persistenceHelper (sumDigits n) (count + 1)
    sumDigits = sumList . digitsOfInt 

-- | digitalRoot n is the digit obtained at the end of the sequence
--   computing the additivePersistence
--
-- >>> digitalRoot 9876
-- 3
digitalRoot :: Int -> Int
-- digitalRoot n = error "TBD"
digitalRoot n
  | n < 10    = n
  | otherwise = digitalRoot (sumDigits n)
  where
    sumDigits = sumList . digitsOfInt


-- | listReverse [x1,x2,...,xn] returns [xn,...,x2,x1]
--
-- >>> listReverse []
-- []
--
-- >>> listReverse [1,2,3,4]
-- [4,3,2,1]
--
-- >>> listReverse ["i", "want", "to", "ride", "my", "bicycle"]
-- ["bicycle", "my", "ride", "to", "want", "i"]

listReverse :: [a] -> [a]
-- listReverse xs = error "TBD"
listReverse xs = reverseAcc xs []   
  where
    reverseAcc :: [a] -> [a] -> [a]  
    reverseAcc []     acc = acc       
    reverseAcc (x:xs) acc = reverseAcc xs (x:acc) 

-- | In Haskell, a `String` is a simply a list of `Char`, that is:
--
-- >>> ['h', 'a', 's', 'k', 'e', 'l', 'l']
-- "haskell"
--
-- >>> palindrome "malayalam"
-- True
--
-- >>> palindrome "myxomatosis"
-- False

palindrome :: String -> Bool
-- palindrome w = error "TBD"
palindrome w = isReverse w w
  where
    isReverse :: String -> String -> Bool
    isReverse [] [] = True
    isReverse (x:xs) (y:ys)
      | x == y = isReverse xs ys  
      | otherwise = False        
    isReverse _ _ = False         


-- | sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30

sqSum :: [Int] -> Int
sqSum xs = foldLeft f base xs
  where
   f a x = a + x^2  
   base  = 0



-- | `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24

pipe :: [(a -> a)] -> (a -> a)
-- pipe fs   = foldLeft f base fs
--   where
--     f a x = error "TBD"
--     base  = error "TBD"
pipe fs = foldLeft f base fs
  where
    f :: (a -> a) -> (a -> a) -> (a -> a)
    f g h x = h (g x)
    
    base :: a -> a
    base = id

-- | `sepConcat sep [s1,...,sn]` returns `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"

sepConcat :: String -> [String] -> String
sepConcat sep []    = ""
--sepConcat sep (h:t) = foldLeft f base l
--  where
--    f a x           = error "TBD"
--    base            = error "TBD"
--    l               = error "TBD"
sepConcat sep (h:t) = foldLeft f base l
  where
    f a x           = if null a then x else a ++ sep ++ x 
    base            = ""  
    l               = h:t  


intString :: Int -> String
intString = show

-- | `stringOfList pp [x1,...,xn]` uses the element-wise printer `pp` to
--   convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1, 2, 3, 4, 5, 6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1, 2, 3], [4, 5], [6], []]"

stringOfList :: (a -> String) -> [a] -> String
-- stringOfList f xs = error "TBD"
stringOfList f xs = "[" ++ sepConcat ", " (map f xs) ++ "]"

-- | `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]
--
-- >>> clone "foo" 2
-- ["foo", "foo"]

clone :: a -> Int -> [a]
-- clone x n = error "TBD"
clone x n
  | n <= 0    = []           
  | otherwise = x : clone x (n - 1) 

type BigInt = [Int]

-- | `padZero l1 l2` returns a pair (l1', l2') which are just the input lists,
--   padded with extra `0` on the left such that the lengths of `l1'` and `l2'`
--   are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- [0,0,9,9] [1,0,0,2]
--
-- >>> padZero [1,0,0,2] [9,9]
-- [1,0,0,2] [0,0,9,9]

padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero l1 l2
  | len1 < len2 = (prependZeros len2 l1, l2)
  | len1 > len2 = (l1, prependZeros len1 l2)
  | otherwise   = (l1, l2)
  where
    len1 = length l1
    len2 = length l2
    prependZeros len xs = zeros len xs ++ xs
    zeros n xs = take (n - length xs) (repeat 0)

-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]
--
-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []

removeZero :: BigInt -> BigInt
removeZero ds = dropWhile (== 0) ds

-- | `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`.
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1, 1, 0, 1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1, 0, 9, 9, 8]

bigAdd :: BigInt -> BigInt -> BigInt
bigAdd l1 l2 = removeZero finalRes
  where
    (l1', l2') = padZero l1 l2
    (finalCarry, res) = foldr f (0, []) (zip l1' l2')
    f (x1, x2) (c, acc) = let s = x1 + x2 + c in (s `div` 10, s `mod` 10 : acc)
    finalRes = case finalCarry of 
                 0 -> res
                 _ -> finalCarry : res


-- | `mulByDigit i n` returns the result of multiplying
--   the digit `i` (between 0..9) with `BigInt` `n`.
--
-- >>> mulByDigit 9 [9,9,9,9]
-- [8,9,9,9,1]

mulByDigit :: Int -> BigInt -> BigInt
mulByDigit i n = 
    let (finalCarry, res) = foldr (\x (c, acc) -> 
                                     let (newCarry, result) = divMod (i * x + c) 10 
                                     in (newCarry, result : acc)) (0, []) n
    in if finalCarry == 0 then removeZero res else finalCarry : res

-- | `bigMul n1 n2` returns the `BigInt` representing the product of `n1` and `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1]

bigMul :: BigInt -> BigInt -> BigInt
-- bigMul l1 l2 = res
--   where
--     (_, res)   = foldRight f base args
--     f x (z, p) = error "TBD"
--     base       = error "TBD"
--     args       = error "TBD"
bigMul l1 l2 = res
  where
    (_, res) = foldr f base args
    f digit (idx, acc) = (idx - 1, bigAdd acc (mulByDigitWithIndex digit idx))
    base = (length l1 - 1, [])
    args = l1
    mulByDigitWithIndex digit idx = appendZeros idx (mulByDigit digit l2)
    appendZeros 0 xs = xs
    appendZeros n xs = 0 : appendZeros (n - 1) xs

-- bigMul l1 l2 = 
--   fst $ foldr f (zeroes, []) (zip l1 [0..])
--  where
--    zeroes = repeat 0
--    f (x1, shift) (z, acc) = 
--     (z, bigAdd (drop shift acc ++ take shift zeroes) (mulByDigit x1 l2))
