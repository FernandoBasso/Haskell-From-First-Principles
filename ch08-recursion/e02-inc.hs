{-# LANGUAGE NoMonomorphismRestriction #-}

inc :: Num a => a -> a
inc = (+ 1)

three :: Num a => a
three = inc . inc . inc $ 0

-- My version.
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes t n = incTimes (t - 1) (n + 1)

-- Book Version.
incTimesBook :: (Eq a, Num a) => a -> a -> a
incTimesBook 0 n = n
incTimesBook times n = 1 + (incTimesBook (times - 1) n)

