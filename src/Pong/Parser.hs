{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Pong.Parser where

import Data.List.NonEmpty (fromList)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import Data.Void
import Pong.Data
import Pong.Lang
import Pong.Util
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Map.Strict as Map

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
keywords = ["if", "then", "else", "match", "let", "in", "lam", "true", "false", "def"
  , "bool"
  , "int" 
  , "float"
  , "double"
  , "char" 
  , "string" ]

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
      f <- parens expr <|> item
      optional (args expr) >>= (fromMaybe [] >>> pure <<< \case
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
  pure (eLam () [toLabel var] e)

caseExpr :: Parser SourceExpr
caseExpr = do
  keyword "match"
  e <- expr
  cs <- braces (optional (symbol "|") >> caseClause `sepBy1` symbol "|")
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
  primString <|> try primFloat <|> primIntegral
  where
    primUnit = symbol "()" $> PUnit
    primTrue = keyword "true" $> PBool True
    primFalse = keyword "false" $> PBool False
    primChar = PChar <$> surroundedBy (symbol "'") printChar
    primString = lexeme (PString . pack <$> chars)
    primFloat = do
      d <- lexeme Lexer.float
      isFloat <- isJust <$> optional (char 'f' <|> char 'F')
      pure $ if isFloat then PFloat (realToFrac d) else PDouble d
    primIntegral = PInt <$> lexeme Lexer.decimal
    chars = char '\"' *> manyTill Lexer.charLiteral (char '\"')

type_ :: Parser Type
type_ = makeExprParser item [[InfixR (tArr <$ symbol "->")]]
  where
    item = keyword "unit" $> tUnit
      <|> keyword "bool" $> tBool
      <|> keyword "int" $> tInt
      <|> keyword "float" $> tFloat
      <|> keyword "double" $> tDouble
      <|> keyword "char" $> tChar
      <|> keyword "string" $> tString
      <|> conType
--      <|> genType
    conType = do
      con <- constructor
      ts <- many type_
      pure (tCon con ts)
--    genType = do
--      -- z
--      v <- identifier
--      undefined

label_ :: Parser (Label Type)
label_ = do
  name <- identifier
  symbol ":"
  t <- type_
  pure (t, name)

def :: Parser (Name, Definition (Label Type) SourceExpr)
def = 
  functionDef -- <|> constantDef <|> externalDef <|> dataDef
    where 
      functionDef = do
        keyword "def"
        name <- identifier
        args <- parens (some label_)
        symbol ":"
        t <- type_
        symbol "="
        e <- expr
        pure (name, Function (fromList args) (t, e))

program :: Parser (Program SourceExpr)
program = do
  defs <- many def
  pure (Program (Map.fromList defs))
