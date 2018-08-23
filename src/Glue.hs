{-# LANGUAGE TemplateHaskell #-}

module Glue (
    glue
  , curryN
) where

import Control.Monad
import Language.Haskell.TH
import Text.PrettyPrint.Leijen.Text

glue :: String -> String
glue = id

-- The language syntax is described here:
-- http://hackage.haskell.org/package/template-haskell-2.13.0.0/docs/Language-Haskell-TH-Syntax.html

curryN :: Int -> Q Exp
curryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

-- curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
-- curry3 f a b c  = f (a, b, c)
--
-- get3 :: [x] -> Maybe (x,x,x)
-- get3 [x1,x2,x3] -> Just (x1,x2,x3)
-- get3 _ -> Nothing
--
--
-- --
-- [sqarql|
--   SELECT ?person ?name
--   WHERE
--     ?person rdf:type "Person" .
--     ?name rdf:name ?name .
--     ?person foaf:knows ${thisguy}
-- |]

-- VarP is a pattern
-- Generally
-- - *P is a pattern element
-- - *T is a type element
-- - *E is an expression

-- AppE Exp Exp := {f x}

-- * lamE - a lambda expression
--
-- -- use to generate unique, unused names with a given basename
-- newName :: String -> Q Name
--
-- -- used here to create n unique names with base "x"
-- replicateM :: Applicative m => Int -> m a -> m [a]
