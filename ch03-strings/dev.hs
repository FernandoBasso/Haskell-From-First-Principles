{-# LANGUAGE NoMonomorphismRestriction #-}

module Reverse where

revStr :: String -> String
revStr str =
  let
    ini :: String
    ini = take 8 str
    mid :: String
    mid = take 2 $ drop 9 str
    end :: String
    end = drop 12 str
  in
    end ++ " " ++ mid ++ " " ++ ini

main :: IO ()
main = print $ revStr "Currying is awesome"
--
-- λ> main
-- "awesome is Currying"
--

