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

