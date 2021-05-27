# Lists - Chapter 09


<!-- vim-markdown-toc GitLab -->

* [Exercise: EnumFromTo](#exercise-enumfromto)
* [Exercises: Thy fearful symmetry](#exercises-thy-fearful-symmetry)
  * [01](#01)
  * [02](#02)
  * [03](#03)
* [Exercises: Comprehend thy lists](#exercises-comprehend-thy-lists)
* [Exercises: Square Cube](#exercises-square-cube)
* [Exercises: Bottom madness](#exercises-bottom-madness)
  * [01](#01-1)
  * [02](#02-1)
  * [03](#03-1)
  * [04](#04)
  * [05](#05)
  * [06](#06)
  * [07](#07)
  * [08](#08)
  * [09](#09)
  * [10](#10)
* [Intermission: Is it in normal form?](#intermission-is-it-in-normal-form)
  * [01](#01-2)
  * [02](#02-2)
  * [03](#03-2)
  * [04](#04-1)
  * [05](#05-1)
  * [06](#06-1)
  * [07](#07-1)
* [Exercises: More bottoms](#exercises-more-bottoms)
  * [01](#01-3)
  * [02](#02-3)
  * [03](#03-3)
  * [04](#04-2)
  * [05](#05-2)
    * [a](#a)
    * [b](#b)
    * [c](#c)
  * [06](#06-2)
* [Exercises: Filtering](#exercises-filtering)
  * [01](#01-4)
  * [02](#02-4)
  * [03](#03-4)
* [Zipping exercises](#zipping-exercises)
  * [01, 02, 03](#01-02-03)
* [Chapter Exercises](#chapter-exercises)
  * [Data.Char](#datachar)
    * [01](#01-5)
    * [02](#02-5)
    * [03](#03-5)
    * [04](#04-3)
    * [05, 06](#05-06)
  * [Ciphers](#ciphers)
  * [Writing your own standard functions](#writing-your-own-standard-functions)
* [The End](#the-end)

<!-- vim-markdown-toc -->

## Exercise: EnumFromTo

Page 306.

```haskell
eftBool :: Bool -> Bool -> [Bool]
eftBool False _ = [False, True]
eftBool True  _ = [True, False]


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd EQ LT = [EQ, LT]
eftOrd EQ GT = [EQ, GT]
eftOrd GT EQ = [GT, EQ]
eftOrd LT _  = [LT, EQ, GT]
eftOrd GT _  = [GT, EQ, LT]

--
-- ‘ini’ must be less than ‘end’. Does not handle randing down, or
-- negative values.
--
eftInt :: Int -> Int -> [Int]
eftInt ini end = go (ini + 1) [ini]
  where go current acc
          | end < current = []
          | current == end = acc ++ [current]
          | otherwise = go (current + 1) (acc ++ [current])


--
-- Only handles ranges “going up”.
--
eftChar :: Char -> Char -> [Char]
eftChar ini end = go (succ ini) [ini]
  where go current acc
          | end < current = []
          | current == end = acc ++ [current]
          | otherwise = go (succ current) (acc ++ [current])
```



## Exercises: Thy fearful symmetry

Page 310.

### 01

```haskell
myWords :: [Char] -> [[Char]]
myWords str = go str []
  where go s acc
          | s == "" = acc
          | head s == ' ' = go (tail s) acc
          | otherwise = go
                        (dropWhile (/= ' ') s)
                        (acc ++ [takeWhile (/= ' ') s])
--
-- [λ> myWords "Tomb Raider - The Angel Of Darkness"
-- ["Tomb","Raider","-","The","Angel","Of","Darkness"]
--
```

### 02

```haskell
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"

sentences = firstSen ++ secondSen
            ++ thirdSen ++ fourthSen

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

dropUntilNL :: [Char] -> [Char]
dropUntilNL str
  | str == "" = str
  | head str == '\n' = tail str
  | otherwise = dropUntilNL (dropWhile (/= '\n') str)

myLines :: String -> [String]
myLines str = go str []
  where go s acc
          | s == "" = acc
          | otherwise = go (dropUntilNL s)
                           (acc ++ [takeWhile (/= '\n') s])

main :: IO ()
main = do
  print $
    "Are they equal? "
    ++ show (myLines sentences == shouldEqual)
```

### 03

```haskell
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"

sentences :: [Char]
sentences = firstSen ++ secondSen
            ++ thirdSen ++ fourthSen

tombRaider :: [Char]
tombRaider = "Tomb Raider - The Angel of Darkness"

shouldEqualSentences :: [[Char]]
shouldEqualSentences =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

shouldEqualTombRaider :: [[Char]]
shouldEqualTombRaider =
  [ "Tomb"
  , "Raider"
  , "-"
  , "The"
  , "Angel"
  , "of"
  , "Darkness"
  ]

dropUntil :: Char -> [Char] -> [Char]
dropUntil chr str
  | str == "" = str
  | head str == chr = tail str
  | otherwise = dropUntil chr (dropWhile (/= chr) str)

toList :: Char -> String -> [String]
toList chr str = go chr str []
  where go chr s acc
          | s == "" = acc
          | otherwise = go chr
                        (dropUntil chr s)
                        (acc ++ [takeWhile (/= chr) s])


main :: IO ()
main = do
  putStrLn $
    "Tomb Raider --: " ++
    (show $ toList ' ' tombRaider == shouldEqualTombRaider)

  putStrLn $
    "Sentences ----: " ++
    (show $ toList '\n' sentences == shouldEqualSentences)

--
-- λ> main
-- Tomb Raider --: True
-- Sentences ----: True
--
```



## Exercises: Comprehend thy lists

Page 314.

Required for the two exercises.

```haskell
mySqr = [x ^ 2 | x <- [1..10]]
```



First exercise:

```haskell
result1 = [x | x <- mySqr, rem x 2 == 0]
```

Using a predicate to filter out numbers that are not even.



Second exercise:

```haskell
result2 = [(x, y) | x <- mySqr,
                    y <- mySqr,
                    x < 50,
                    y > 50]
```

Combine each `x` with all `y`s.

## Exercises: Square Cube

Page 316.

```haskell
mySqr :: (Num a, Enum a) => [a]
mySqr = [x ^ 2 | x <- [1..5]]

myCube :: (Num a, Enum a) => [a]
myCube = [y ^ 3 | y <- [1..5]]

tup1 :: [(Integer, Integer)]
tup1 = [(x, y) | x <- mySqr, y <- myCube]

tup2 :: [(Integer, Integer)]
tup2 = [(x, y) | x <- mySqr,
                 y <- myCube,
                 x < 50,
                 y < 50]

qty :: Int
qty = length tup2
```

## Exercises: Bottom madness

Page 325.

### 01

Bottom because each element of the first generator will be applied to each element of the second generator.

### 02

Produces `[1]` instead of bottom because of laziness and the fact that `take 1` does not require the list to be evaluated up to `undefined`.

### 03

Bottom because `sum` is strict on the values.

### 04

Works fine because `length` only cares about the spine.

### 05

Because we concatenate the list with `undefined`, it makes part of the spine, crashing `length`.

### 06

Produces `[2]`. Does not reach `undefined` because of `take 1`.

### 07

We reach `undefined` before finding and even number, causing a crash (bottom).

### 08

We are able to produce one value before reaching undefined.

### 09

We are able to produce two values before hitting undefined.

### 10

Bottom. Reach undefined before being able to produce three valid values.

## Intermission: Is it in normal form?

### 01

NF, WHNF.

The list is fully evaluated and all its values are known.

### 02

WHNF.

The list is not fully known because it has the `_` hole.

### 03

Neither. The expression is a function fully applied which has not yet been evaluated.

### 04

Idem

### 05

Idem

### 06

Idem. `++` is a fully applied function but its operands have not bee fully evaluated yet.

### 07

WHNF.

It is a data constructor and one of its arguments is still unknown (the `_`).

## Exercises: More bottoms

Page 332.

### 01

Bottom. We take 1, but the first evaluated element of the list is ‘undefined’.

### 02

Produces the value 2. This time, we still take 1, but ‘undefined’ is the second element of the list.

### 03

Bottom. We take 2, and ‘undefined’ appears as the second element of the list.

### 04

The type signatures means it maps a list of Char to a list of Bool.

It produces a new list of `Bool` . Lowercase vowels are `True`, other chars, `False`

```haskell
f :: [Char] -> [Bool]
f xs = map (\x -> elem x "aeiou") xs
```

### 05

#### a

Use sectioning for the `^` function so each element of the list is to the left of the `^`. Takes each element of the list to the power of 2.

```ghci
λ> map (^ 2) [1..10]
[1,4,9,16,25,36,49,64,81,100]
```

#### b

Produces a new list containing the minimum (lowest) value of each inner list.

```ghci
λ> map minimum [[1..10], [10..20], [20..30]]
[1,10,20]
```

#### c

Produces a list with the sums of each inner list.

```ghci
λ> map sum [[1..5], [1..5], [1..5]]
[15,15,15]
```

### 06

With `bool`, if the third argument is `False`, return the first argument, othewerwise, return the second argument.

```ghci
λ> import Data.Bool (bool)
λ> map (\x -> bool (x + 100) (- x) (x == 3)) [1 .. 5]
[101,102,-3,104,105]
```



## Exercises: Filtering

Page 335.

### 01

```ghci
λ> filter (\n -> rem n 3 == 0) [1..30]
[3,6,9,12,15,18,21,24,27,30]

λ> [n | n <- [1..30], rem n 3 == 0]
[3,6,9,12,15,18,21,24,27,30]
```

### 02

```haskell
multOf :: Int -> [Int] -> [Int]
multOf n genList =
  filter (\x -> rem x n == 0) genList

multOf3 :: [Int] -> [Int]
multOf3 = multOf 3

-- OK
r1 = (length . multOf3) [1..30]

-- OK
r2 = (length . multOf 3) [1..30]

-- Oops.
r3 = (length . multOf) 3 [1..30]
```

Looks like function composition in Haskell does not syntax sugar “one argument per function” to appear it takes multiple arguments. A composed chain of functions takes only one argument (unlike Ramda.js, for example, in which the first function in the chain can take as many arguments as necessary.)

### 03

```haskell
s1 = "the brown dog was a goof"
s2 = "there is an old jedi here"

dropArticles :: [Char] -> [[Char]]
dropArticles str = filter isNotArticle $ words str
  where isNotArticle w = not $ elem w ["a", "an", "the"]
--
-- λ> dropArticles s1
-- ["brown","dog","was","goof"]
-- λ> dropArticles s2
-- ["there","is","old","jedi","here"]
--
```

## Zipping exercises

Page 337.

### 01, 02, 03

```haskell
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _  []         = []
myZip (x:xs) (y:ys) = [(x, y)] ++ myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _          = []
myZipWith _ _  []         = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' [] _          = []
myZip' _  []         = []
myZip' (x:xs) (y:ys) =
  (myZipWith (,) [x] [y]) ++ myZip' xs ys

myZip'' :: [a] -> [b] -> [(a, b)]
myZip'' [] _          = []
myZip'' _  []         = []
myZip'' xs ys =
  (myZipWith (,) xs ys) ++ myZip'' (tail xs) (tail ys)
```

## Chapter Exercises

### Data.Char

Page 338.

Assume this for all the solutions:

```haskell
import Data.Char
```

#### 01

```haskell
λ> :t toUpper
toUpper :: Char -> Char
λ> :t isUpper
isUpper :: Char -> Bool
```

#### 02

Using function composition:

```haskell
dropUppers :: [Char] -> [Char]
dropUppers = filter $ not . isUpper
```

#### 03

```haskell
capitFst :: [Char] -> [Char]
capitFst str = [toUpper . head $ str] ++ tail str

-- More elegant. Uses pattern-matching, which
-- simplifies the body.
capitFst' :: [Char] -> [Char]
capitFst' (x:xs) = [toUpper x] ++ xs
```

#### 04

```haskell
capit :: [Char] -> [Char]
capit [] = []
capit (x:xs) = [toUpper x] ++ capit xs
```

#### 05, 06

```haskell
-- The exercise wants us to use ‘head’ instead of
-- pattern-matching on the first cons cell of the spine.

-- Using pattern-matching anyway.
upFst :: [Char] -> Char
upFst (x:_) = toUpper x

upFst' :: [Char] -> Char
upFst' s = toUpper $ head s

upFst'' s = toUpper . head $ s

upFst''' = toUpper . head
```

### Ciphers

Page 339.

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

module Chipher where

import Data.Char

--
-- Where ‘mod’ should wrap around. In this case, it is the
-- length of the alphabet plus 1.
--
wrap :: Int
wrap = ord 'z' - ord 'a' + 1 -- 26

--
-- From an alphabet ranging from 0 to 25, ‘a’ is 0, ‘b’ is
-- 1, ‘z’ is 25.
--
pos :: Char -> Int
pos c = ord c - ord 'a'


rotate :: (Int -> Int -> Int) -> Int -> Char -> Char
rotate _ _ ' ' = ' '
rotate f step c   = newChar
  where
    newPos = mod (pos c `f` step) wrap
    newChar = chr (newPos + ord 'a')


caesar :: Int -> [Char] -> [Char]
caesar step str = map (rotate (-) step) str


unCaesar :: Int -> [Char] -> [Char]
unCaesar step str = map (rotate (+) step) str


orig = "the quick brown fox jumps over the lazy dog"
caesared = "qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald"


main :: IO ()
main = do
  print $ caesar 3 orig == caesared
  print $ unCaesar 3 caesared == orig
```

### Writing your own standard functions

Page 341.

```haskell
myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs


myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs) = e == x || myElem e xs


myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (e ==)


myRev :: [a] -> [a]
myRev []     = []
myRev (x:xs) = myRev xs ++ [x]


squish :: [[a]] -> [a]
squish []            = []
squish (xs:listOfXs) = xs ++ squish listOfXs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs
--
-- λ> squishMap (\x -> "  __" ++ [x] ++ "__") "abc"
-- "  __a__  __b__  __c__"
--

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain listOfXs = squishMap (\xs -> xs ++ []) listOfXs
--
-- λ> squishAgain [[1..3], [4..6]]
-- [1,2,3,4,5,6]
--

squishAgainPF :: [[a]] -> [a]
squishAgainPF = squishMap (\xs -> xs ++ [])
--
-- λ> squishAgain [[1..3], [4..6]]
-- [1,2,3,4,5,6]
--


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy compFn (x:xs) =
  case compFn x (myMaximumBy compFn xs) of
    GT -> x
    EQ -> x
    LT -> myMaximumBy compFn xs


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy compFn (x:xs) =
  case compFn x (myMinimumBy compFn xs) of
    LT -> x
    EQ -> x
    GT -> myMinimumBy compFn xs


myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare
```



















## The End
