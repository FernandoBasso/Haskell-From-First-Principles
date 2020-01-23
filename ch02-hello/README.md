# Hello Haskell, chapter 02



## Exercises: Comprehension Check

Page 35.

### 01, playing with exprs

```ghci
λ> half x = (/) x 2
λ> square x = (*) x x

λ> square (half 3)
2.25
```



### 02 writing a function

```ghci
λ> f n = 3.14 * (n * n)
λ> f 5
78.5
λ> f 10
314.0
λ> f 2
12.56
λ> f 4
50.24
```

3.14 does not change. That value is hardcoded inside the function body. The value that is multiplied inside parentheses do change, so, we make it a parameter.

### 03 function using `pi'

```ghci
λ> f n = pi * (n * n)
λ> f 2
12.566370614359172
λ> f 4
50.26548245743669
```

## Exercises: Parentheses and Association

Page 39.

### 01 add, multiply

a and b produce different results. Parentheses here does make a difference.

### 02 perimeter, multiply and add

No change. Parenthesizing multiplication around an addition does not change anything. The multiplication would have been performed first anyway.

### 03 divide and add

Here, doing 2 + 9 before the division does change the result.

## Exercises: Heal the Sick

Page 45.

### 01 area space after dot

There is a space after the dot in "3. ".

```haskell
area x = 3.14 * (x * x)
```

### 02 unbound variable, unused variable

The function bounds `x` but attempts to use `b`, which is not in scope. Fix: use `x` inside the body of the function:

```haskell
double x = x * 2
```

### 03 indentation mishap

There is a horrible, monstrous whitespace before `y` causing GHCi to throw a fit.

```haskell
x = 7
y = 10
f = x + y
```





https://www.quora.com/How-do-I-use-the-dollar-sign-separator-in-Haskell

https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign



## Exercises: A Head Code

Page 59.

### let expressions

#### 01 let

`let x = 5 in x` produces 5. The `in x` is like a return statement in this case. We bind 5 to `x` and “return” x.

#### 02 let

Similar to the previous one. Produce `x * x`, which is 25.

#### 03 let

Produces 30. We use `let` to bind two values for `x` and `y`, and both are in scope for the `in` clause.

#### 04 let

Similar to the previous one. Just that the `in` clause ignores `y`, and the result is 6.

### Rewrite to where

#### 01 where

```haskell
ex1 = result
  where x       = 3
        y       = 1000
        result  = x * 3 + y
```

#### 02 where

```haskell
ex2 = result
  where
    y       = 10
    x       = 10 * 5 + y
    result  = x * 5
```

#### 03 where

```haskell
ex3 = result
  where
    x       = 7
    y       = negate x
    z       = y * 10
    result  = z / x + y
```



## The End