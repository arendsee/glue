{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Glue.Substitute (
    substitute
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Text.PrettyPrint.Leijen.Text  as Gen
import Text.Parsec
import Text.Parsec.Char

substitute :: QuasiQuoter
substitute = QuasiQuoter {
    quoteExp  = compile
  , quotePat  = error "Can't handle patterns"
  , quoteType = error "Can't handle types"
  , quoteDec  = error "Can't handle declarations"
  }

compile :: String -> Q Exp
compile s = case parse pIs "" s of
  Left err -> error $ show err
  Right xs -> return $ AppE (VarE 'Gen.hsep) (ListE (map qI xs)) where
    qI :: I -> Exp
    qI (S s) = AppE (VarE 'Gen.string) (LitE (StringL s))
    qI (V s) = VarE (mkName s)


type Parser = Parsec String ()

data I = S String | V String

pIs :: Parser [I]
pIs = many1 (pV <|> pS) <* eof

pV :: Parser I
pV = fmap V $ char '$' >> between (char '{') (char '}') (many1 (noneOf "}")) 

pS :: Parser I
pS = fmap S $ many1 (noneOf "$")
