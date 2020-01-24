{-# LANGUAGE NoMonomorphismRestriction #-}

module PrintFlipped where

myGreeting :: String
myGreeting = (++) "Hello" " world"

hello :: [Char]
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting =
          (++) hello ((++) " " world)
          -- hello ++ " " ++ world

