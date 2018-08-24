{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Glue.Perl (
    perl
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Text.PrettyPrint.Leijen.Text  as Gen
import Text.Parsec
import Text.Parsec.Char

perl :: QuasiQuoter
perl = QuasiQuoter {
    quoteExp  = compile
  , quotePat  = error "Can't handle patterns"
  , quoteType = error "Can't handle types"
  , quoteDec  = error "Can't handle declarations"
  }

compile :: String -> Q Exp
compile s = case parse pIs "" s of
  Left err -> error $ show err
  Right xs -> return $ AppE (VarE 'Gen.hcat) (ListE (map qI xs)) where
    qI :: I -> Exp
    qI (S s) = AppE (VarE 'Gen.string) (LitE (StringL s))
    qI (V s) = VarE (mkName s)


type Parser = Parsec String ()

data I = S String | V String

pIs :: Parser [I]
pIs = many1 (pS <|> pV) <* eof

pV :: Parser I
pV = fmap V $ (char '$') >> (many1 (alphaNum)) 

pS :: Parser I
pS = fmap S $ many1 (try (char '\\' >> char '$') <|> noneOf "$")
