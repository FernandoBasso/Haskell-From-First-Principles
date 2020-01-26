# Basic Datatypes - Chapter 4

## Exercises: Mood Swing

Page 90.

1: `Mood`

2: `Blah` and `Woot`.

3: The type signature should use `Mood`, the type constructor, not `Woot`, which is a data constructor.

```haskell
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- 
--     λ> changeMood Woot
--     Blah
--     λ> changeMood Blah
--     Woot
-- 
```

5: Dutifully done!

## Exercises: Find the Mistakes

Page 103.

1. `true` is not capitalized. Make it `True`.
2. To compare values, we use `==`, not `=` (the later is used to define things, not to compare things).
3. This one is correct.
4. Probably wanted double quotes around Merry and Happy. Other possibility is that they are data constructors from a typeclass that implements `Eq` and `Ord`.
5. Cannot concatenate two lists with elements of different types. In this case, either compare two lists of numbers, or two lists of strings.

## Chapter Exercises

Page 111.

### 01 length

Page 112.

`length` takes one argument: a list of any type. It returns a number (`Int`) which is the number of elements in the list.

```ghci
λ> :type length
length :: Foldable t => t a -> Int

λ> :info length
class Foldable (t :: * -> *) where
  ...
  length :: t a -> Int
  ...
  	-- Defined in ‘Data.Foldable’
```

`Foldable t => t a` means `[[]]`.

### 02

A: 5, because the list contains five numbers.

B: 3, because the list contains three two-tuples.

C: 2, because `allAwesome` contains two lists (which themselves contains other lists, but `length` cares about the topmost elements, and it is two lists).

D: 5, because `concat` _flattens_ the lists, making one single list with the three elements of `awesome` and the other two elements of `also`.

### 03 length division

`6 / 3` is okay, because both 6 and 3 are polymorphic constants. When `/` is applied, then both 6 and 3 are assumed to be of the `Fractional` typeclass, because `/` is defined in the `Fractional` typeclass.

`6 / length [1, 2, 3]` does not work because length returns `Int`, and `/` takes types that have instances of the `Fractional` typeclass, which `Int` simply does not.

### 04

Could use `div` instead because it would work for the polymorphic constant 6 and the return of `length`, which is `Int`.

```ghci
λ> div 6 $ length [1..3]
2
λ> div 6 (length [1..3])
2
```

While 6 is a polymorphic number, it can be "converted” to `Int`, which is the return of `lengt`. But the return of `Int` cannot be converted to a fractional type because `/` only take fractional operands.

### 05

The type of the entire expression is `Bool` and the expected result is `True`, one of `Bool` data constructors.

```ghci
λ> :type 2 + 3 == 5
2 + 3 == 5 :: Bool
λ> 2 + 3 == 5
True
```

### 06

`x` is a `Num`, not a concrete type (remember GHCi does not infer a concrete type until it is forced to, at the last moment). The entire expression type is `Bool`, and the returned value is `False`, one of `Bool` data constructors.

```ghci
λ> x = 5
λ> :type x
x :: Num p => p
λ> :type x + 3 == 5
x + 3 == 5 :: Bool
λ> x + 3 == 5
False
```

### 07 will they work?

- `length allAwesome == 2` works and produces `True`. The function application has the highest precedence. After the application produces 2, comparing it with the other 2 produces `True`.
- `length [1, 'a', 3, 'b']` fails, producing an error. All elements of a list must be of the same type.
- `length allAwesome + length awesome` works and produces 5. First the two ` length` applications will return 2 and 3 respectively. They have higher precedence than `(+)`, which will add those two `Int` values together to produce 5.
- `(8 == 8) && ('b' < 'a'` works and produces `False`. 8 is equal to 8, so it is true. 'b’ is not less than (comes before than) 'a’, so it is false. Then `True && False` is evaluated, which produces `False`, the final result.
- `(8 == 8) && 9` fails, producing an error. The value 9 is not one of the possible values of the type `Bool`, required by `&&`.

### 08 palindromoe

```haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome s = s == reverse s
-- λ> isPalindrome "racecar"
-- True
-- λ> isPalindrome "ana"
-- True
-- λ> isPalindrome "banana"
-- False
```

### 09 abs of a num

```haskell
myAbs :: Integer -> Integer
myAbs n = if n < 0 then -n else n
-- λ> myAbs (-5)
-- 5
-- λ> myAbs 5
-- 5
```

### 10 tuples

```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))
-- λ> f ("Lara", "Tomb") ("Croft", "Raider")
-- (("Tomb","Raider"),("Lara","Croft"))
```

### Correcting Syntax

Page 114.

#### 01 fix syntax and uppercase fn name

The name of the function has to start with a lowercase.

`x` was defined to be `(+)`, and now `x` is a prefix function. To use it in infix position, surround it with back ticks, not single quotes.

Working solution:

```haskell
f xs = w `x` 1
  where w = length xs
```

#### 02 uppercase binding name

Can’t use uppercase as first letter of identifiers for variables (and functions, for that matter). Correct:

```ghci
\x -> x
```

#### 03 A not in scope

Need a comma between a and b in the tuple param. Also,`A` is interpreted as a data constructor (because it starts with uppercase), but it is not in scope. Fix:

```haskell
f (a, b) = a
```

### Match the function names to their types

#### 01 type of `show'

A: Wrong. Type constructors do not start with lowercase.

B: Wrong. The first `->` should be an `=>`.

C: Correct.

#### 02 Type of `(==)'

A: Wrong. We can’t assume `a`, without any type constraints will have an instance of something that provides `==`.

B: Correct.

C: Wrong. The first `->` should be a `=>`.

D: Wrong. `a` should not be capitalized, and still, missing a second `a`.

#### 03 Type of `fst'

A: Correct.

B: Nonsense.

C: Wrong. It is the type of `snd`, not `fst`.

#### 04 type of `(+)'

A: Wrong. First `->` should be a `=>` and  `(+)` does not return `Bool`.

B: Wrong. Almost fine, except `(+)` does not return `Bool`.

C: Wrong. Typeclasses are capitalized.

D: Correct. Properly return the typeclass-constrained `a`.

D: Wrong. `a` is lacking a typeclass constraint of `Num`.







## The End