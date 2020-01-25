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



## The End