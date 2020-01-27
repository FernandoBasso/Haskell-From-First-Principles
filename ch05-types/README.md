# Types - Chapter 05



## Exercises: Type Matching

Page 127.

### 01 functions

- A matches C.
- B matches D.
- C matches B.
- D matches A.
- E matches E.



## Exercises: Type Arguments

Page 136.

### 01 type of  f x

A is correct.

### 02 type of g 0 'c'

D is correct. We applied 0 for `a`, `'c'` for `b`, and `"woot"` for `c`. So, `b` is `Char`. Since we applied all three arguments, and `b` is the return type, `Char` is the return type.

### 03 type of h 1.0 2

D is correct. The compiler is not forced yet to give a concrete type to 2.

### 04 type of h 1 (5.5 :: Double)

C is correct because we now told the compiler that `b` is `Double`.

### 05 jackal

A is correct. The param for `a` is `[Char]`, so, the return `a` must also be `[Char]`.

### 06 jackal

E is correct. Only one param was supplied, so, it still needs the `b` in order to return the `a`.

### 07 kessel

A is correct. `a` initially had a type constraint of `Ord`, but since we passed a number, now `a` has to be `Ord` but also `Num` (because it could be `Ord` and `Char`,  for instance).

```ghci
λ> :type kessel 1 2
kessel 1 2 :: (Ord a, Num a) => a

λ> :type kessel 'k' 2
kessel 'k' 2 :: Char
```

2 is polymorphic, so, `Num a` makes sense. But `'k'` is not polymorphic, it is the concrete type `Char`.

### 08 kessel

A is correct. 1 does not have a concrete type so the compiler still consider it to be the most generic and polymorphic possible numeric type.

### 09 kessel

C is correct. Now, the argument 1 was explicitly declared to be `Integer`, so, the return `a` is `Integer`.

## Exercises: Parametricity

Page 142.

### 01 id

Impossible.

“That is why you fail.” -- Master Yoda

### 02 a -> a -> a

```haskell
f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

-- λ> f1 "Lara" "Croft"
-- "Lara"
-- λ> f2 "Lara" "Croft"
-- "Croft"
```

### 03 a -> b -> b

Only one possible implementation.

```haskell
f :: a -> b -> b
f j k = k
-- λ> f "One" "Two"
-- "Two"
```



## Exercises: Apply Yourself

Page 174.

### 01 (++)

Then it will change to `myConcat :: [Char] -> [Char]`.

### 02 (/)

It must be some sort  of `Fractional` because of the `(/)` function.

```
myMult :: Fractional a => a -> a
```

### 03 take

Applying it to `[Char]` must mean it takes an `Int` (because of `take`) and produce `[Char]`.

```
myTake :: Int -> [Char]
```

### 04 >

Since `length` gives an `Int`, `x` must be an `Int` too. The return is `Bool` because `>` produces `Bool`.

```
myCom :: Int -> Bool
```

### 05 <

Since `<` is applied to a `Char`, `x` must also be `Char`. Returns `Bool`.

```
myAlph :: Char -> Bool
```



## Chapter Exercises

Page 150.

### Multiple choice

Page 150.

#### 01 type of [a]

C.

#### 02 type [[a]] -> [a]

A.

#### 03 type [a] -> Int -> a

B.

#### 04 type (a, b) -> a

C.

### Determine the type

Page 151.

#### 01 values and types returned

A: 54, `Num a`.

B: `(0, "doge")`, a tuple of `Num a => (a, [Char])`.

C: `(0, "doge")`, a tuple of `(Integer, [Char])`. Not that the type class constraint is gone because `Integer` is a concrete type, and was set explicitly.

D: `False`, return type is `Bool`.

E: 5, return is `Int`.

F: `False`, return is `Bool`.

#### 02 type of w

It is the polymorphic typeclass `Num a` because nothing forced the compiler to assign a more concrete type.

#### 03 type of z

It is `z :: Num a => a -> a`. The compiler sees nothing hinting that the param and return should be a more concrete type. It knows it is `Num a` though because of the `(+)` function which comes from the `Num` typeclass.

#### 04 type of f

It is `f :: Fractional a => a -> a`. Again, no information can be inferred that could lead to a more concrete type, but because of `(/)`, it has to be `Fractional` since `(/)` is defined in the `Fractional` typeclass (and not in `Num`, for instance).

#### 05 type of f ++

It is `f :: [Char]` because `++` is used to join lists, and we are passing the concrete type of `Char` for the list elements, so the compile figures it is a list of concrete type, a list of `Char`.

### Does it compile?

Page 152.

#### 01 bigNum wahoo

The first line compiles and produces an irreducible expression. We cannot then get that value (which is not a function by the way) and attempt to apply it to `$ 10`. So, the second line does not compile. `bigNum` is a number, and we can't apply a number to other expressions.

#### 02 print

Yes, it does.

#### 03 (+) function

Doesn't compile. We can't apply `b`, which isn't a function, to 10.

#### 04 scope and order of evaluation

In a file (not in the REPL), `b` defined on the second line can be used just fine on the first line, but `c` is not defined and not in scope. Doesn't compile.

### Type variable or specific type constructor?

Page 153.

The choices are: a fully polymorphic type variable, a constrained polymorphic type variable, or a concrete type constructor.

#### 01 Num a b Int

constrained polymorphic, fully polymorphic, concrete, concrete

#### 02 zed blah

fully polymorphic, concrete, concrete.

`Zed` and `Blah` are concrete (and not constrained polymorphic) because they are not accompanied by a variable, like `Zed a =>` and `Blah b =>`.

#### 03 Enum a b C

fully polymorphic, constrained polymorphic, concrete.

#### 04 f g C

fully polymorphic, fully polymorphic, concrete

### Write a type signature

Page 153.

####  01 fnH

```haskell
fnH :: [a] -> a
fnH (x:_) = x
```

This is the same as `head`.

#### 02 fnC

```haskell
fnC :: Ord a => a -> a -> Bool
fnC x y =
  if (x > y)
  then True
  else False
```

The function itself could be as simple as this, by the way:

```haskell
fnC :: Ord a => a -> a -> Bool
fnC x y = x > y
```

#### 04 fnS

```haskell
fnS :: (a, b) -> b
fnS (x, y) = y
```

### Given a type, write a function

Page 154.

#### 01 id

```haskell
i :: a -> a
i x = x
```

Same as the identity function.

#### 02 first

```haskell
c :: a -> b -> a
c x y = x
```

Like the curried tuple `fst` function. Given two values, returns the first.

####  03 alpha equiv

Yes, same thing.

#### 04 snd

```haskell
c' :: a -> b -> b
c' x y = y
```

Same as the curried tuple `snd` function. Given two values, returns the second/last.

#### 05 reverse, tail

One is the “reverse” function:

```haskell
r :: [a] -> [a]
r [] = []
r (x:xs) = r xs ++ [x]
-- λ> r "hello"
-- "olleh"
--
-- or simply ‘r = reverse’.
```

Another is the “tail” function.

```haskell
r :: [a] -> [a]
r (x:xs) = xs
-- λ> r "hey"
-- "ey"
--
-- Or simply ‘r = tail’
```

#### 06 aToB aToC

```haskell
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB x = bToC (aToB x)
```

Looks like a function that would convert from one type to another.

#### 07 ignore fn param

```haskell
a :: (a -> c) -> a -> a
a xToY x = x
```

Takes a function as the first param but simply ignores it, just returning the second param.

#### 08 aToB

```haskell
a' :: (a -> b) -> a -> b
a' xToY x = xToY x
```

### Fix it

Page 156.

#### 04 module Sing

- `sing` should be capitalized, since it is the name of a module.

- In the type signature for `fstString`, replace `++` with `->`.

- In the type signature for `sndString`, replace `Char` with `[Char]`.

- It is not “or”. Use  `else` instead.

- Two `x` definitions. Make one be `y` instead, since it is even used in the program.

Fixed:

```haskell
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y)
       then fstString x
       else sndString y
  where x = "Singin"
        y = "Somewhere"
```

#### 02 change song

Just replace `>` with `<` in the definition of `sing`.

#### 03 arith broken

- `Main` should not be capitalized since it is the name of a function.
- `print 1 + 2` produces an error because function application has the highest precedence, only 1 is passed to `print` and the `+ 2` becomes nonsense. Make it `print (1 + 2)` or `print $ 1 + 2`.
- Can't `putStrLn` on a number. Make it a string first with `show`.
- Can't use `-1` like that in that position. Surround it with parens or make it `negate $ negate 1` or `negate (negate 1)`.
- The other two lines are fine. 

Fixed:

```haskell
module Arith where

main :: IO ()
main = do
  print $ 1 + 2
  putStrLn $ show 10
  print (negate $ negate 1)
  print ((+) 0 blah)
  where blah = negate 1
```

### Type-Kwon-Do

Page 157.

Example from the book:

```haskell
data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, f w)
```

#### 01 int to str to char

```haskell
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = g (f i)
```

#### 02 A to B to C

```haskell
q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)
```

#### 03 xform, X to Y to Z

```haskell
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)
```

#### 04 munge

```haskell
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZ x =
  fst $ yToWZ (xToY x)
```









