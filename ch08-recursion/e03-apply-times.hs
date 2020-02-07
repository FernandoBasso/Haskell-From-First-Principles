{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- NOTE: Doesn't work with negative numbers because we are doing
-- ‘times - 1’, so, if ‘times’ is negative, we never reach zero.
-- Rather, we continue going to further negative numbers, never
-- reaching the base case when times is zero.
--

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f acc = acc
-- applyTimes times f acc = f (applyTimes (times - 1) f acc)
applyTimes times f initial = f . applyTimes (times - 1) f $ initialo

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times initial = applyTimes times (+ 1) initial

