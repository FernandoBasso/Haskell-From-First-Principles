{-# LANGUAGE NoMonomorphismRestriction #-}

module FnWithWhere where

printInc n = print plusTwo
  where plusTwo = n + 2
