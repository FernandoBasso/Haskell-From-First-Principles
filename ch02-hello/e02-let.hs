{-# LANGUAGE NoMonomorphismRestriction #-}

module FnWithLet where

printInc n =
  let plusTwo = n + 2
  in print plusTwo

printInc' n =
  let plusTwo x = x + 2
  in print $ plusTwo n
