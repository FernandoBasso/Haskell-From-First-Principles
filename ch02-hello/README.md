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

