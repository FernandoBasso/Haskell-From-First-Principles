# Type Classes - Chapter 06



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





