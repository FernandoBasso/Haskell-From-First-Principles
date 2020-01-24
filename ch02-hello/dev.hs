{-# LANGUAGE NoMonomorphismRestriction #-}

waxOn =
  let
    z = 7
    y = z + 8
    x = y ^ 2
  in x * 5

triple n = n * 3

waxOff n = triple n

waxOff' n = triple $ (/) n 2
