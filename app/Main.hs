{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Glue
import qualified Text.PrettyPrint.Leijen.Text as P

foo :: (Int, Int, Int) -> Int
foo (a,b,c) = a + b + c

main :: IO ()
main = do
  print $  $(curryN 3) foo 1 3 5

-- main = P.putDoc [doc|hi ${you}|] where
--   you = "Bob Filthermyster"
