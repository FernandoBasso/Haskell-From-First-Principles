{-# LANGUAGE NoMonomorphismRestriction #-}

data Div =
    Res Integer
  | ByZero
  deriving Show

divby :: Integer -> Integer -> (Div, Div)
divby num denom = go num denom 0
  where go n d count
          | n < d = (Res count, Res n)
          | otherwise = go (n - d) d (count + 1)

negFst :: (Div, Div) -> (Div, Div)
negFst (Res x, Res y) = (Res (- x), Res y)

negSnd :: (Div, Div) -> (Div, Div)
negSnd (Res x, Res y) = (Res x, Res (- y))

--
-- Do the subtraction with the absolute values, and then negate the
-- tuple constituents according to the division rules.
--

dv :: Integer -> Integer -> (Div, Div)
dv num denom
  | denom == 0 = (ByZero, ByZero)
  | num < 0 && denom < 0 = negSnd $ go (-num) (-denom) 0
  | num < 0 && denom > 0 = negFst . negSnd $ go (-num) denom 0
  | num > 0 && denom < 0 = negFst $ go num (-denom) 0
  | denom < 0 = negSnd $ go num (-denom) 0
  | otherwise = go num denom 0
  where go n d acc
          | n < d = (Res acc, Res n)
          | otherwise = go (n - d) d (acc + 1)

