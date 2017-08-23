module IGE.Lisp where

import Protolude hiding ((<$>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (pack)
import Text.PrettyPrint.Leijen.Text hiding (parens, empty)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme = L.lexeme sc
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

data SExp = Atom Text | List [SExp]
  deriving (Show, Eq)

sexpParser :: Parser SExp
sexpParser = (fmap List $ parens (many sexpParser)) <|> (fmap Atom $ lexeme atom)

atom :: Parser Text
atom = fmap pack $ some (alphaNumChar <|> symbolChar)

pprintSexp :: SExp -> Doc
pprintSexp (List sexps) = text "(" <> hsep (map pprintSexp sexps) <> text ")"
pprintSexp (Atom a) = textStrict a
