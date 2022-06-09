{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Pong.Read where

import Control.Monad.Combinators.Expr
import qualified Control.Newtype.Generics as Generics
import Data.Functor (($>))
import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Tuple.Extra (first, second)
import Data.Void (Void)
import Pong.Data
import Pong.Lang
import Pong.Util (Name, (<<<), (>>>))
import Text.Megaparsec hiding (token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

spaces :: Parser ()
spaces =
  Lexer.space
    space1
    (Lexer.skipLineComment "--")
    (Lexer.skipBlockComment "{-" "-}")

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

{-# INLINE symbol #-}
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaces

parens :: Parser a -> Parser a
parens = symbol "(" `between` symbol ")"

brackets :: Parser a -> Parser a
brackets = symbol "[" `between` symbol "]"

braces :: Parser a -> Parser a
braces = symbol "{" `between` symbol "}"

surroundedBy :: Parser Text -> Parser a -> Parser a
surroundedBy parser = between parser parser

commaSep :: Parser a -> Parser [a]
commaSep parser = parser `sepBy` symbol ","

commaSep1 :: Parser a -> Parser [a]
commaSep1 parser = parser `sepBy1` symbol ","

{-# INLINE args #-}
args :: Parser a -> Parser [a]
args = parens . commaSep

{-# INLINE args1 #-}
args1 :: Parser a -> Parser [a]
args1 = parens . commaSep1

keywords :: [Text]
keywords =
  [ "if"
  , "then"
  , "else"
  , "match"
  , "letr"
  , "let"
  , "in"
  , "lam"
  , "true"
  , "false"
  , "def"
  , "const"
  , "extern"
  , "bool"
  , "int"
  , "float"
  , "double"
  , "char"
  , "string"
  ]

keyword :: Text -> Parser ()
keyword tok = Megaparsec.string tok *> notFollowedBy alphaNumChar *> spaces

word :: Parser Text -> Parser Text
word parser =
  lexeme $
    try $ do
      name <- parser
      if name `elem` keywords
        then fail ("Reserved keyword " <> unpack name)
        else pure name

validChar :: Parser Char
validChar = alphaNumChar <|> char '_'

withInitial :: Parser Char -> Parser Text
withInitial parser = do
  c <- parser
  chars <- many validChar
  pure (pack (c : chars))

constructor :: Parser Name
constructor = word (withInitial upperChar)

identifier :: Parser Name
identifier = word (withInitial (lowerChar <|> char '_'))

{-# INLINE toLabel #-}
toLabel :: t -> ((), t)
toLabel = ((),)

expr :: Parser SourceExpr
expr = makeExprParser apps operator
  where
    apps = do
      f <- parens (expr <|> spaces $> eLit PUnit) <|> item
      optional (args expr)
        >>= ( fromMaybe [] >>> pure <<< \case
                [] -> f
                as -> eApp () f as
            )
    item =
      litExpr
        <|> ifExpr
        <|> resExpr
        <|> letExpr
        <|> lamExpr
        <|> matchExpr
        <|> recExpr
        <|> varExpr
        <|> conExpr

fix7, fix6, fix4, fix3, fix2 :: [Operator Parser SourceExpr]
fix7 = [InfixL (eOp2 ((), OMul) <$ symbol "*")]
fix6 =
  [ InfixL (eOp2 ((), OAdd) <$ try (symbol "+" <* notFollowedBy (symbol "+")))
  , InfixL (eOp2 ((), OSub) <$ symbol "-")
  ]
fix4 =
  [ InfixN (eOp2 ((), OEq) <$ symbol "==")
  , InfixN (eOp2 ((), ONEq) <$ symbol "/=")
  , InfixN (eOp2 ((), OLt) <$ try (symbol "<" <* notFollowedBy (symbol "=")))
  , InfixN (eOp2 ((), OGt) <$ try (symbol ">" <* notFollowedBy (symbol "=")))
  , InfixN (eOp2 ((), OLtE) <$ symbol "<=")
  , InfixN (eOp2 ((), OGtE) <$ symbol ">=")
  ]
fix3 = [InfixR (eOp2 ((), OLogicAnd) <$ symbol "&&")]
fix2 = [InfixR (eOp2 ((), OLogicOr) <$ symbol "||")]

operator :: [[Operator Parser SourceExpr]]
operator =
  [ fix7
  , fix6
  , fix4
  , fix3
  , fix2
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

matchExpr :: Parser SourceExpr
matchExpr = do
  keyword "match"
  e <- expr
  cs <- braces (optional (symbol "|") >> caseClause `sepBy1` symbol "|")
  pure (ePat e cs)

resExpr :: Parser SourceExpr
resExpr = do
  keyword "letr"
  f <- braces $ do
    lhs <- identifier
    symbol "="
    rhs <- identifier
    symbol "|"
    row <- identifier
    pure [toLabel lhs, toLabel rhs, toLabel row]
  e1 <- symbol "=" *> expr
  e2 <- symbol "in" *> expr
  pure (eRes f e1 e2)

caseClause :: Parser ([Label ()], SourceExpr)
caseClause = do
  ls <- do
    con <- constructor
    ids <- fromMaybe [] <$> optional (args identifier)
    pure (toLabel <$> con : ids)
  symbol "=>"
  e <- expr
  pure (ls, e)

recExpr :: Parser SourceExpr
recExpr =
  braces $ do
    fields <- commaSep field
    tail <- optional (symbol "|" *> identifier)
    pure $
      eRec
        ( case fields of
            [] -> rNil
            _ -> foldr (uncurry rExt) (maybe rNil (rVar . toLabel) tail) fields
        )
  where
    field = do
      lhs <- identifier
      symbol "="
      rhs <- expr
      pure (lhs, rhs)

prim :: Parser Prim
prim =
  primUnit
    <|> primTrue
    <|> primFalse
    <|> primChar
    <|> primString
    <|> try primFloat
    <|> try primDouble
    <|> primIntegral
  where
    primUnit = symbol "(" $> symbol ")" $> PUnit
    primTrue = keyword "true" $> PBool True
    primFalse = keyword "false" $> PBool False
    primChar = PChar <$> surroundedBy (symbol "'") printChar
    primString = lexeme (PString . pack <$> chars)
    primFloat = do
      f <- lexeme (Lexer.float <* (char 'f' <|> char 'F'))
      pure (PFloat f)
    primDouble = do
      d <- lexeme Lexer.float
      pure $ PDouble (realToFrac d)
    primIntegral = PInt <$> lexeme Lexer.decimal
    chars = char '\"' *> manyTill Lexer.charLiteral (char '\"')

scheme :: Parser Scheme
scheme = Scheme <$> type_

type_ :: Parser (Type Void Name)
type_ =
  makeExprParser (parens item <|> item) [[InfixR (tArr <$ symbol "->")]]
  where
    item =
      keyword "unit" $> tUnit
        <|> keyword "bool" $> tBool
        <|> keyword "int" $> tInt
        <|> keyword "float" $> tFloat
        <|> keyword "double" $> tDouble
        <|> keyword "char" $> tChar
        <|> keyword "string" $> tString
        <|> conType
        <|> genType
    conType = do
      con <- constructor
      ts <- many type_
      pure (tCon con ts)
    genType =
      tGen <$> identifier

arg :: Parser (Label (Type Void Name))
arg = do
  name <- identifier
  symbol ":"
  t <- type_
  pure (t, name)

def :: Parser (Label Scheme, Definition () SourceExpr)
def = functionDef <|> constantDef <|> externalDef -- <|> dataDef -- TODO
  where
    functionDef = do
      keyword "def"
      name <- identifier
      args <- parens (commaSep1 arg)
      symbol ":"
      t <- type_
      symbol "="
      e <- expr
      let ts = fst <$> args
          fun = Function (fromList (first (const ()) <$> args)) ((), e)
      pure ((Scheme (foldType t ts), name), fun)

    constantDef = do
      keyword "const"
      name <- identifier
      symbol ":"
      t <- type_
      symbol "="
      e <- expr
      pure ((Scheme t, name), Constant ((), e))

    externalDef = do
      keyword "extern"
      name <- identifier
      symbol ":"
      t <- type_
      let names = Map.fromList (Set.toList (boundVars t) `zip` (tVar <$> [0 ..]))
          t0 = toMonoType names t
      pure ((Scheme t, name), Extern (argTypes t0) (returnType t0))

program :: Parser (Program () SourceExpr)
program = do
  defs <- many def
  pure (Program (Map.fromList defs))

parseProgram :: Text -> Either ParserError (Program () SourceExpr)
parseProgram = runParser program ""
