# Recursion - Chapter 08


<!-- vim-markdown-toc GitLab -->

* [Intermission: Exercise](#intermission-exercise)
* [Chapter Exercises](#chapter-exercises)
  * [Review of types](#review-of-types)
    * [01](#01)
    * [02](#02)
    * [03](#03)
    * [04](#04)
  * [Reviewing currying](#reviewing-currying)
    * [01](#01-1)
    * [02](#02-1)
    * [03](#03-1)
    * [04](#04-1)
  * [Recursion](#recursion)
    * [01](#01-2)
    * [02](#02-2)
    * [03](#03-2)
  * [Fixing divBy](#fixing-divby)
  * [wordNumber](#wordnumber)

<!-- vim-markdown-toc -->

## Intermission: Exercise

Page 282.

```haskell
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes t f acc = f . applyTimes (t - 1) f $ acc
applyTimes 5 (+1) 5
```

Evaluation:

```
applyTimes 5    (+ 1)     5
		(5 - 1)        (5 + 1)
		   4              6

applyTimes 4    (+ 1)     6
		(4 - 1)        (6 + 1)
		   3              7

applyTimes 3    (+ 1)     7
		(3 - 1)        (7 + 1)
		   2              8

applyTimes 2    (+ 1)     8
		(2 - 1)        (8 + 1)
		   1              9

applyTimes 1    (+ 1)     9
		(1 - 1)        (9 + 1)
		   0              10

applyTimes 0 (+ 1) 10
				   10
```

mvaldesdeleon has a [different solution](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch08/exercises.md).

## Chapter Exercises

Page 293.

### Review of types

#### 01

D is correct.

#### 02

B is correct.

#### 03

D is correct.

#### 04

B is correct.

### Reviewing currying

#### 01

`String`.

#### 02

`String`.

#### 03

`String`.

#### 04

`String`.

### Recursion

#### 01

Will do `divby 7 2` instead. 15 is just too much trouble :|

```
divby   7     2
go      7     2     0
go      5     2     1
go      3     2     2
go      1     2     3
go      (3, 1)
```

#### 02

```haskell
sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo n = go n 0
   where go n acc
           | n == 0 = acc
           | otherwise = go (n - 1) (acc + n)
```

#### 03

```haskell
mult :: Integral a => a -> a -> a
mult x y = go x y 0
  where go n m acc
          | m == 0 = acc
          | otherwise = go n (m - 1) (acc + n)
```

### Fixing divBy

```haskell
data Div =
    Res Integer
  | ByZero
  deriving Show

negFst :: (Div, Div) -> (Div, Div)
negFst (Res x, Res y) = (Res (- x), Res y)

negSnd :: (Div, Div) -> (Div, Div)
negSnd (Res x, Res y) = (Res x, Res (- y))

--
-- Do the subtraction with the absolute values, and then negate the
-- tuple constituents according to the division rules.
--

divBy :: Integer -> Integer -> (Div, Div)
divBy num denom
  | denom == 0 = (ByZero, ByZero)
  | num < 0 && denom < 0 = negSnd $ go (-num) (-denom) 0
  | num < 0 && denom > 0 = negFst . negSnd $ go (-num) denom 0
  | num > 0 && denom < 0 = negFst $ go num (-denom) 0
  | denom < 0 = negSnd $ go num (-denom) 0
  | otherwise = go num denom 0
  where go n d acc
          | n < d = (Res acc, Res n)
          | otherwise = go (n - d) d (acc + 1)
```

### wordNumber

```haskell
module WordNumber where

import Data.List (intersperse)

snums :: [[Char]]
snums = [
  "zero",
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
  ]

--
-- A list of [(1, "one"), ... (9, "nine")].
--
tups :: [(Int, [Char])]
tups = zip [(0 :: Int) .. 9] snums

--
-- Turns a digit into a word, like 1 -> "one" or 7 -> "seven".
--
digitToWord :: Int -> String
digitToWord n = word
  where word =
          snd . head $ filter (\ tup -> fst tup == n) tups

--
-- Turns a number into a list of individual digits. Ex:
-- 1984 -> [1, 9, 8, 4].
--
digits :: Int -> [Int]
digits n = go n []
  where
    go x acc
      | x < 10 = [x] ++ acc
      | otherwise = go (div x 10) ([mod x 10] ++ acc)

--
-- Makes use of the previously defined functions to wordify a number.
--
wordNumber :: Int -> String
wordNumber n =
  concat . intersperse "-" $ map digitToWord $ digits n
--
-- λ> wordNumber 0
-- "zero"
-- λ> wordNumber 1984
-- "one-nine-eight-four"
--
```



