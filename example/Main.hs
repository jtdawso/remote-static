{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Distributed.Closure

import Data.Binary
import Data.Word
import Data.Typeable
import GHC.StaticPtr

              
fib :: Int -> Int
fib n  
  | n > 1 = (fib (n-1)) + (fib (n-2))
  | n <= 1 && n >= 0 = n

printInt :: Int -> IO()
printInt n = print n

readFile' :: FilePath -> IO String
readFile' = readFile


fibPtr :: StaticPtr (Int -> Int)
fibPtr = static fib


main :: IO ()
main = do
  print $ staticPtrInfo fibPtr
  putStrLn "Static Key: "
  print $ staticKey fibPtr
  let bs = encode $ staticKey fibPtr
  f <- unsafeLookupStaticPtr (decode bs)
  case f of
    Just func -> print $  (deRefStaticPtr func:: Int->Int) 5
    Nothing -> error "Unable to lookup static ptr"
  let fibC = closure fibPtr
  let totc = cap fibC (closure $ static 8)
  let t = typeRep fib
  let w =  unclosure (decode (encode totc))
  printInt w

  putStrLn $ "Fib using printInt:"
  printInt $  fib 0
  printInt $  fib 1
  printInt $  fib 2
  printInt $  fib 3
  printInt $  fib 4
  printInt $  fib 5
  printInt $  fib 6
  printInt $  fib 7
  printInt $  fib 8
  putStrLn "readFile':"
  (print . lines)  =<< readFile' "helloWorld.txt"
