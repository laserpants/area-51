{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Pong.Parser where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void
import Pong.Data
import Pong.Lang
import Pong.Util
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

spaces :: Parser ()
spaces =
  Lexer.space
    space1
    (Lexer.skipLineComment "--")
    (Lexer.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaces

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

surroundedBy :: Parser Text -> Parser a -> Parser a
surroundedBy parser = between parser parser

commaSep :: Parser a -> Parser [a]
commaSep parser = parser `sepBy` symbol ","

commaSep1 :: Parser a -> Parser [a]
commaSep1 parser = parser `sepBy1` symbol ","

fields :: Parser a -> Parser [a]
fields = braces . commaSep

args :: Parser a -> Parser [a]
args = parens . commaSep

args1 :: Parser a -> Parser [a]
args1 = parens . commaSep1

keywords :: [Text]
keywords = ["if", "then", "else", "case", "let", "in", "lam", "true", "false"]

keyword :: Text -> Parser ()
keyword tok = Megaparsec.string tok *> notFollowedBy alphaNumChar *> spaces

word :: Parser Text -> Parser Text
word parser =
  lexeme $
  try $ do
    name <- parser
    if name `elem` keywords
      then fail ("Reserved word " <> unpack name)
      else pure name

validChar :: Parser Char
validChar = alphaNumChar <|> char '_'

withInitial :: Parser Char -> Parser Text
withInitial parser = do
  c <- parser
  cs <- many validChar
  pure (pack (c : cs))

constructor :: Parser Name
constructor = word (withInitial upperChar)

identifier :: Parser Name
identifier = word (withInitial (lowerChar <|> char '_'))

toLabel :: t -> ((), t)
toLabel = ((), )

type SourceExpr = Expr () () () Void

expr :: Parser SourceExpr
expr = makeExprParser apps operator
  where
    apps = do
      f <- parens item <|> item
      optional (args item) >>= (fromMaybe [] >>> pure <<< \case
        [] -> f
        as -> eApp () f as)
    item =
      litExpr <|> ifExpr <|> letExpr <|> lamExpr <|> caseExpr <|>
      rowExpr <|>
      varExpr <|>
      conExpr

operator :: [[Operator Parser SourceExpr]]
operator
    -- 7
 =
  [ [InfixL (eOp2 (Op2 OMul ()) <$ symbol "*")]
      -- 6
  , [ InfixL (eOp2 (Op2 OAdd ()) <$ try (symbol "+" <* notFollowedBy (symbol "+")))
    , InfixL (eOp2 (Op2 OSub ()) <$ symbol "-")
    ]
      -- 4
  , [InfixN (eOp2 (Op2 OEq ()) <$ symbol "==")]
      -- 3
  , [InfixR (eOp2 (Op2 OLogicAnd ()) <$ symbol "&&")]
      -- 2
  , [InfixR (eOp2 (Op2 OLogicOr ()) <$ symbol "||")]
  ]

varExpr :: Parser SourceExpr
varExpr = eVar . toLabel <$> identifier

conExpr :: Parser SourceExpr
conExpr = eCon . toLabel <$> constructor

litExpr :: Parser SourceExpr
litExpr = eLit <$> prim

ifExpr :: Parser SourceExpr
ifExpr = do
  e1 <- keyword "if" *> expr
  e2 <- keyword "then" *> expr
  e3 <- keyword "else" *> expr
  pure (eIf e1 e2 e3)

letExpr :: Parser SourceExpr
letExpr = do
  name <- keyword "let" *> identifier
  e1 <- symbol "=" *> expr
  e2 <- symbol "in" *> expr
  pure (eLet (toLabel name) e1 e2)

lamExpr :: Parser SourceExpr
lamExpr = do
  keyword "lam"
  var <- parens identifier
  e <- symbol "=>" *> expr
  pure (eLam [toLabel var] e)

caseExpr :: Parser SourceExpr
caseExpr = do
  keyword "match"
  e <- expr
  cs <- braces (caseClause `sepBy1` symbol "|")
  pure (eCase e cs)

caseClause :: Parser ([Label ()], SourceExpr)
caseClause = do
  ls <- conPattern <|> rowPattern <|> varPattern
  symbol "=>"
  e <- expr
  pure (ls, e)
  where
    conPattern = do
      con <- constructor
      ids <- fromMaybe [] <$> optional (args identifier)
      pure (toLabel <$> con : ids)
    rowPattern =
      braces $ do
        lhs <- identifier
        symbol "="
        rhs <- identifier
        symbol "|"
        row <- identifier
        pure [toLabel ("{" <> lhs <> "}"), toLabel rhs, toLabel row]
    varPattern = do
      var <- identifier
      pure [toLabel var]

rowExpr :: Parser SourceExpr
rowExpr =
  braces $ do
    fs <- fields field
    tail <- optional (symbol "|" *> identifier)
    let row =
          case fs of
            [] -> rNil
            _ -> foldr (uncurry rExt) (maybe rNil (rVar . toLabel) tail) fs
    pure (eRow row)
  where
    field = do
      lhs <- identifier
      symbol "="
      rhs <- expr
      pure (lhs, rhs)

prim :: Parser Prim
prim =
  primUnit <|> primTrue <|> primFalse <|> primChar <|>
  primString -- <|> try parseFloat
   <|>
  primIntegral
  where
    primUnit = symbol "()" $> PUnit
    primTrue = keyword "true" $> PBool True
    primFalse = keyword "false" $> PBool False
    primChar = PChar <$> surroundedBy (symbol "'") printChar
    primString = lexeme (PString . pack <$> chars)
    primFloat = undefined -- TDouble <$> lexeme Lexer.float
    primIntegral = PInt <$> lexeme Lexer.decimal -- TODO
    chars = char '\"' *> manyTill Lexer.charLiteral (char '\"')
