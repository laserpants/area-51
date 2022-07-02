{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Pong.Read where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (pack, unpack)
import Data.Tuple.Extra (first)
import Data.Void (Void)
import Pong.Data
import Pong.Lang
import Pong.Util (Name, Text, project, (<&>), (>>>))
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

{-# INLINE symbol_ #-}
symbol_ :: Text -> Parser ()
symbol_ = void . symbol

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
  , "field"
  , "let"
  , "in"
  , "lam"
  , "true"
  , "false"
  , "func"
  , "not"
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
        <&> ( \case
                Nothing -> f
                Just [] -> eApp () f [eLit PUnit]
                Just as -> eApp () f as
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

fix8, fix7, fix6, fix4, fix3, fix2 :: [Operator Parser SourceExpr]
fix8 =
  [ Prefix (eOp1 ((), ONot) <$ keyword "not")
  , Prefix (eOp1 ((), ONeg) <$ symbol "-")
  ]
fix7 =
  [ InfixL (eOp2 ((), OMul) <$ symbol "*")
  , InfixL (eOp2 ((), ODiv) <$ (symbol "/" <* notFollowedBy (symbol "=")))
  ]
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
  [ fix8
  , fix7
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
  cs <- braces (optional (symbol "|") >> matchClause `sepBy1` symbol "|")
  pure (ePat e cs)

resExpr :: Parser SourceExpr
resExpr = do
  keyword "field"
  f <- braces $ do
    lhs <- identifier
    symbol_ "="
    rhs <- identifier
    symbol_ "|"
    row <- identifier
    pure [toLabel lhs, toLabel rhs, toLabel row]
  e1 <- symbol "=" *> expr
  e2 <- symbol "in" *> expr
  pure (eRes f e1 e2)

matchClause :: Parser ([Label ()], SourceExpr)
matchClause = do
  ls <- do
    con <- constructor
    ids <- fromMaybe [] <$> optional (args identifier)
    pure (toLabel <$> con : ids)
  symbol_ "=>"
  e <- expr
  pure (ls, e)

recExpr :: Parser SourceExpr
recExpr = do
  fs <- commaSep (field "=" expr)
  tl <- optional (symbol "|" *> identifier)
  pure (foldr (uncurry eExt) (maybe eNil (eVar <$> toLabel) tl) fs)

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
    primUnit = symbol "(" *> spaces *> symbol ")" $> PUnit
    primTrue = keyword "true" $> PBool True
    primFalse = keyword "false" $> PBool False
    primChar = PChar <$> surroundedBy (symbol "'") printChar
    primString = lexeme (PString . pack <$> chars)
    primFloat = do
      f <- lexeme (Lexer.float <* (char 'f' <|> char 'F'))
      pure (PFloat f)
    primDouble = do
      d <- lexeme (Lexer.float :: Parser Double)
      pure $ PDouble (realToFrac d)
    primIntegral = PInt <$> lexeme Lexer.decimal
    chars = char '\"' *> manyTill Lexer.charLiteral (char '\"')

scheme :: Parser Scheme
scheme = Scheme <$> polyType

{- ORMOLU_DISABLE -}

polyType :: Parser (Type Name)
polyType =
  makeExprParser (parens item <|> item) [[InfixR (tArr <$ symbol "->")]]
  where
    item =
      keyword "unit"         $> tUnit
        <|> keyword "bool"   $> tBool
        <|> keyword "int"    $> tInt
        <|> keyword "float"  $> tFloat
        <|> keyword "double" $> tDouble
        <|> keyword "char"   $> tChar
        <|> keyword "string" $> tString
        <|> recType
        <|> genType
        <|> conType
    conType = do
      con <- constructor
      ts  <- many polyType
      pure (tCon con ts)
    genType =
      tVar <$> identifier
    recType =
      tRec
        <$> braces
          ( do
              fs <- commaSep (field ":" polyType)
              tl <- optional (symbol "|" *> identifier)
              pure (foldr (uncurry rExt) (maybe rNil tVar tl) fs)
          )

{- ORMOLU_ENABLE -}

field :: Text -> Parser a -> Parser (Name, a)
field sep parser = do
  lhs <- identifier
  symbol_ sep
  rhs <- parser
  pure (lhs, rhs)

arg :: Parser (Label (Type Name))
arg = do
  name <- identifier
  symbol_ ":"
  t <- polyType
  pure (t, name)

definition :: Parser (Label Scheme, Definition () SourceExpr)
definition = functionDef <|> constantDef <|> externalDef <|> typeDef
  where
    functionDef = do
      keyword "func"
      name <- identifier
      as <- parens (commaSep1 arg)
      symbol_ ":"
      t <- polyType
      symbol_ "="
      e <- expr
      let ts = fst <$> as
          fun = Function (fromList (first (const ()) <$> as)) ((), e)
      pure ((Scheme (foldType t ts), name), fun)

    constantDef = do
      keyword "const"
      name <- identifier
      symbol_ ":"
      t <- polyType
      symbol_ "="
      e <- expr
      pure ((Scheme t, name), Constant ((), e))

    externalDef = do
      keyword "extern"
      name <- identifier
      symbol_ ":"
      t <- polyType
      let names = Map.fromList (Set.toList (boundVars t) `zip` [0 ..])
          t0 = toMonoType names t
      pure ((Scheme t, name), Extern (argTypes t0) (returnType t0))

    typeDef = do
      keyword "type"
      name <- constructor
      tvs <- many identifier <* symbol "="
      let t = tCon name (tVar <$> tvs)
      cons <- dataCon `sepBy` symbol "|"
      pure ((Scheme t, name), Data name cons)

    dataCon = do
      polyType
        >>= ( project
                >>> \case
                  TCon con ts -> pure (Constructor con ts)
                  _ -> fail "Not a constructor"
            )

module_ :: Parser (Module () SourceExpr)
module_ = do
  defs <- many definition
  pure (Module (Map.fromList defs))

parseModule :: Text -> Either ParserError (Module () SourceExpr)
parseModule = runParser module_ ""
