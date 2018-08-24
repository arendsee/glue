{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Main where

import Glue
import qualified Text.PrettyPrint.Leijen.Text as P

foo :: (Int, Int, Int) -> Int
foo (a,b,c) = a + b + c

you = "Bob Filthermyster"

main :: IO ()
main = do
  print $  $(curryN 3) foo 1 3 5
  P.putDoc [verbatim|hi ${you}
      voodooo  

|]
  P.putDoc [substitute|hi ...${you}...
      voodooo } 

|]
  P.putDoc [perl|hi $you ... I have \$5 in my pocket
|]
