# Type Classes - Chapter 06


<!-- vim-markdown-toc GitLab -->

* [Exercises: Eq instances](#exercises-eq-instances)
  * [01 TisAnInteger](#01-tisaninteger)
  * [02 TwoIntegers](#02-twointegers)
  * [03 StringOrInt](#03-stringorint)
  * [04 Pair](#04-pair)
  * [05 Tuple](#05-tuple)
  * [06 Which](#06-which)
  * [07 EitherOr](#07-eitheror)
* [Exercises: Will they work?](#exercises-will-they-work)
  * [01 max and length](#01-max-and-length)
  * [02 compare](#02-compare)
  * [03 compare](#03-compare)
  * [04 >](#04-)
* [Chapter Exercises](#chapter-exercises)
  * [Multiple choice](#multiple-choice)
    * [01 Eq class](#01-eq-class)
    * [02 Ord type class](#02-ord-type-class)
    * [03 Ord > operator](#03-ord-operator)
    * [04 divMod](#04-divmod)
  * [Does it typecheck](#does-it-typecheck)
    * [01 IO () Person](#01-io-person)
    * [02 Mood](#02-mood)
    * [03 settleDown Mood](#03-settledown-mood)
    * [04 Sentence](#04-sentence)
  * [Given a datatype declaration, what can we do?](#given-a-datatype-declaration-what-can-we-do)
    * [01](#01)
    * [02](#02)
    * [03](#03)
    * [04](#04)
  * [Match The Types](#match-the-types)
    * [01 Float a](#01-float-a)
    * [02 Float Num](#02-float-num)
    * [03 Float Fractional](#03-float-fractional)
    * [04 Float RealFrac](#04-float-realfrac)
    * [05 a Ord](#05-a-ord)
    * [06 a Int](#06-a-int)
    * [07 Int a](#07-int-a)
    * [08 Int Num](#08-int-num)
    * [09 Ord [a] [Int]](#09-ord-a-int)
    * [10 [Char] to Ord a => [a]](#10-char-to-ord-a-a)
    * [11 mySort [Char]](#11-mysort-char)
  * [Type-Known-Do Two : Electric typealoo](#type-known-do-two-electric-typealoo)
    * [01 chk](#01-chk)
    * [02 arith](#02-arith)

<!-- vim-markdown-toc -->

## Exercises: Eq instances

Page 181.

### 01 TisAnInteger

```haskell
data TisAnInteger = TisAnInteger

instance Eq TisAnInteger where
  (==) TisAnInteger TisAnInteger = True
```

It is redundant to implement a catch-all with `_` placeholder. `TisAnInteger` type has only one data constructor, which is also named `TisAnInteger`. Also, defining `==` gives us `/=` for free as well.

### 02 TwoIntegers

```haskell
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = (==) x x' && (==) y y'
```

### 03 StringOrInt

```haskell
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt n) (TisAnInt n') = (==) n n'
  (==) (TisAString s) (TisAString s') = (==) s s'
  (==) _ _ = False
```

### 04 Pair

```haskell
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') =
    (==) x x' && (==) y y'
```

### 05 Tuple

```haskell
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') =
    (==) x x' && (==) y y'
```

### 06 Which

```haskell
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = (==) x x'
  (==) (ThatOne x) (ThatOne x') = (==) x x'
  (==) _           _            = False
```

### 07 EitherOr

```haskell
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x)   (Hello y)   = (==) x y
  (==) (Goodbye x) (Goodbye y) = (==) x y
  (==) _           _           = False
```

## Exercises: Will they work?

### 01 max and length

Works. Produces 5.

### 02 compare

Works. Produces `LT`.

### 03 compare

Doesn't work. Can't compare values of different types.

### 04 >

Works. Produces `False`.

## Chapter Exercises

Page 208.

### Multiple choice

Page 208.

#### 01 Eq class

C is correct.

#### 02 Ord type class

B is correct.

#### 03 Ord > operator

A is correct.

#### 04 divMod

C is correct.

### Does it typecheck

Page 210.

#### 01 IO () Person

It does not type check. It does if we add a `deriving Show` at the end of the type declaration. Fix:

```haskell
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn $ show person
```

#### 02 Mood

Does not type check. Can’t use `==` without an instance of `Ord`. Fix:

```haskell
data Mood =
  Blah | Woot
  deriving (Eq, Show)

settleDown x =
  if x == Woot
  then Blah
  else x
```

#### 03 settleDown Mood

a. Only `Blah` and `Woot` because those are the only two inhabitants of that type.

b. 9 is not accepted because it is not a valid data constructor for `Mood`, the inferred type for `settleDown`.

c. Will not typecheck unless `Mood` has an instance of `Ord`.

#### 04 Sentence

Yes, it does. Just be careful with `s1` because it is a Sentence still awaiting one argument.

### Given a datatype declaration, what can we do?

#### 01

`Papu` does not take a string and a bool, but a `Rocks` and a `Yeah`. Something like this works:

```haskell
phew = Papu (Rocks "chases") (Yeah True)
```

#### 02

Typechecks!

#### 03

Works because `Papu` derives `Eq` and `Papu`'s data constructors uses types which also implement `Eq`.

#### 04

Does not typecheck  because `Papu` and its data constructor types do not implement `Ord`.

### Match The Types

#### 01 Float a

After `i = 1`, `i` has to be some sort of number, it cannot be simply the fully parametrically type `a`. Cannot drop the typeclass constraint.

#### 02 Float Num

Can’t change the type signature from `f :: Float` to `f :: Num a => a` because `Num` does not imply `Fractional`. 1.0 is a value that can be one of the `Fractional` concrete types like `Float` or `Double`, and `Num` does not. Cannot relax from `Fractional` to `Num`.

#### 03 Float Fractional

Contrary to the situation above now, we changed the signature from `Float` to `Fractional`, which works because 1.0 has to be one of the fractional types.

#### 04 Float RealFrac

Works because both `Double` and `Float` implement `RealFrac`.

#### 05 a Ord

Works because we are making the type more specific.

#### 06 a Int

Works because we are making the type more specific.

#### 07 Int a

Doesn’t work. Can’t make the type more generic because `myX` is of a very specific type.

#### 08 Int Num

Same as the previous. Can’t make the function any more generic.

#### 09 Ord [a] [Int]

Works. We can make the function more specific.

#### 10 [Char] to Ord a => [a]

Works. `sort` requires `Ord => a -> [a]` and `head` requires `[a]`.

#### 11 mySort [Char]

Doesn’t work because we use `mySort` which requires `[Char]`, which is more specific than `Ord a => [a]`.

### Type-Known-Do Two : Electric typealoo

Page 214.

#### 01 chk

```haskell
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (==) (aToB a) b
```

#### 02 arith

```haskell
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i n = (+) (fromInteger i) (aToB n)
```

```ghci
λ> :type fromInteger
fromInteger :: Num a => Integer -> a
```

Since `b` is `Num`, and we have a function `(a -> b)`, and the return is `b`, we cannot just add `i` and `(aToB a)` together. We must first make `i` become `Num` so `(+)` works on two `Num` operands.
