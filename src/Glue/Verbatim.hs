{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Glue.Verbatim (
    verbatim
) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.PrettyPrint.Leijen.Text

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
