{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Glue (
    curryN
  , verbatim
) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.PrettyPrint.Leijen.Text

curryN :: Int -> Q Exp
curryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

verbatim :: QuasiQuoter
verbatim = QuasiQuoter {
    quoteExp  = compile
  , quotePat  = error "Can't handle patterns"
  , quoteType = error "Can't handle types"
  , quoteDec  = error "Can't handle declarations"
  }
  where
    compile :: String -> Q Exp
    compile s = return $ AppE (VarE 'string) (LitE (StringL s))
