{-# LANGUAGE NoMonomorphismRestriction #-}

module TopOrLocal where

topLevelFn :: Integer -> Integer
topLevelFn n =
  n + woot + topLevelVal
  where woot :: Integer
        woot = 10

topLevelVal :: Integer
topLevelVal = 5

