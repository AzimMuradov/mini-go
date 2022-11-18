{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Ast
import ConstExprSimplification (simplifyConstExpr)
import Control.Applicative.Combinators (between)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text, concat)
import Data.Void (Void)
import Lexer hiding (Parser)
import Text.Megaparsec (MonadParsec (..), Parsec, choice, eitherP, many, optional, parseMaybe, sepBy, some, (<|>))
import Text.Megaparsec.Char (char)

-- Parser entry point

parse :: Parser a -> Text -> Maybe a
parse = parseMaybe

type Parser = Parsec Void Text

-- Program

fileP :: Parser Ast.Program
fileP = sc *> programP <* eof

programP :: Parser Ast.Program
programP = do
  decls <- many topLevelDeclP
  let vars = [x | Left x <- decls]
  let funcs = [x | Right x <- decls]
  return Ast.Program {Ast.topLevelVarDecls = vars, Ast.topLevelFunctionDefs = funcs}

topLevelDeclP :: Parser (Either Ast.VarDecl Ast.FunctionDef)
topLevelDeclP = eitherP (stmtVarDeclP <* semicolon) functionDefP

-- Expression

expressionListP :: Parser [Ast.Expression]
expressionListP = sepBy expressionP comma

expressionP :: Parser Ast.Expression
expressionP = makeExprParser expressionTerm opsTable

expressionTerm :: Parser Ast.Expression
expressionTerm =
  choice'
    [ parens expressionP,
      Ast.ExprLiteral <$> literalP,
      Ast.ExprIdentifier <$> choice' [identifierP, stdlibFuncP]
    ]

opsTable :: [[Operator Parser Ast.Expression]]
opsTable =
  [ [funcCallOp, arrayAccessByIndexOp],
    [ unaryOp "+" Ast.UnaryPlusOp,
      unaryOp "-" Ast.UnaryMinusOp,
      unaryOp "!" Ast.NotOp,
      unaryOp "^" Ast.BitwiseComplementOp
    ],
    [ mulOp "*" Ast.MultOp,
      mulOp "/" Ast.DivOp,
      mulOp "%" Ast.ModOp,
      mulOp "<<" Ast.BitShiftLeftOp,
      mulOp ">>" Ast.BitShiftRightOp,
      mulOp "&^" Ast.BitClearOp,
      mulOp "&" Ast.BitAndOp
    ],
    [ addOp "+" Ast.PlusOp,
      addOp "-" Ast.MinusOp,
      addSpOp "|" Ast.BitOrOp,
      addOp "^" Ast.BitXorOp
    ],
    [ relOp "==" Ast.EqOp,
      relOp "!=" Ast.NeOp,
      relOp "<=" Ast.LeOp,
      relOp "<" Ast.LtOp,
      relOp ">=" Ast.MeOp,
      relOp ">" Ast.MtOp
    ],
    [andOrOp "&&" Ast.AndOp],
    [andOrOp "||" Ast.OrOp]
  ]

-- Operator Types

binary :: Text -> [Text] -> (Ast.Expression -> Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
binary name [] fun = InfixL $ fun <$ symbol name
binary name nfbNames fun = InfixL $ fun <$ (notFollowedBy (choice' $ symbol <$> nfbNames) *> symbol name)

prefix :: Text -> (Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
prefix name fun = Prefix $ fun <$ symbol name

postfix :: Parser (Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
postfix = Postfix

-- Operators

andOrOp :: Text -> Ast.BinaryOp -> Operator Parser Ast.Expression
andOrOp name op = binary name [] (Ast.ExprBinaryOp op)

relOp :: Text -> Ast.RelOp -> Operator Parser Ast.Expression
relOp name op = binary name [] (Ast.ExprBinaryOp (Ast.RelOp op))

addOp :: Text -> Ast.AddOp -> Operator Parser Ast.Expression
addOp name op = binary name [] (Ast.ExprBinaryOp (Ast.AddOp op))

addSpOp :: Text -> Ast.AddOp -> Operator Parser Ast.Expression
addSpOp name op = binary name ["||"] (Ast.ExprBinaryOp (Ast.AddOp op))

mulOp :: Text -> Ast.MulOp -> Operator Parser Ast.Expression
mulOp name op = binary name [] (Ast.ExprBinaryOp (Ast.MulOp op))

unaryOp :: Text -> Ast.UnaryOp -> Operator Parser Ast.Expression
unaryOp name op = prefix name (Ast.ExprUnaryOp op)

funcCallOp :: Operator Parser Ast.Expression
funcCallOp = postfix $ do
  args <- listed expressionP comma
  return $ flip Ast.ExprFuncCall args

arrayAccessByIndexOp :: Operator Parser Ast.Expression
arrayAccessByIndexOp = postfix $ do
  indexExpr <- brackets expressionP
  index <- do
    case simplifyConstExpr indexExpr of
      Just (Ast.LitInt len) -> return len
      _ -> fail "this is not a const int expression"
  return $ \arr -> Ast.ExprArrayAccessByIndex arr index

-- Type

typeP :: Parser Ast.Type
typeP =
  choice'
    [ Ast.TInt <$ idInt,
      Ast.TBool <$ idBool,
      Ast.TString <$ idString,
      -- TODO : arrayTypeP,
      functionTypeP,
      parens typeP
    ]

functionTypeP :: Parser Ast.Type
functionTypeP = do
  void kwFunc
  params <- typesP
  result <- choice' [typesP, maybeToList <$> optional' typeP]
  return $ Ast.TFunction params result
  where
    typesP = listed typeP comma

-- Function definition

functionDefP :: Parser Ast.FunctionDef
functionDefP = Ast.FunctionDef <$ kwFunc <*> identifierP <*> functionSignatureP <*> stmtBlockP

functionSignatureP :: Parser Ast.FunctionSignature
functionSignatureP = do
  params <- listed paramP comma
  result <- choice' [listed typeP comma, maybeToList <$> optional' typeP]
  return $ Ast.FunctionSignature params result
  where
    paramP = (,) <$> identifierP <*> typeP

-- Statements

statementP :: Parser Ast.Statement
statementP =
  choice'
    [ stmtReturnP,
      stmtBreakP,
      stmtContinueP,
      -- TODO : stmtForP,
      Ast.StmtVarDecl <$> stmtVarDeclP,
      Ast.StmtIfElse <$> stmtIfElseP,
      Ast.StmtBlock <$> stmtBlockP,
      Ast.StmtSimple <$> simpleStmtP
    ]

stmtReturnP :: Parser Ast.Statement
stmtReturnP = Ast.StmtReturn <$ kwReturn <*> expressionListP

stmtBreakP :: Parser Ast.Statement
stmtBreakP = Ast.StmtBreak <$ kwBreak

stmtContinueP :: Parser Ast.Statement
stmtContinueP = Ast.StmtContinue <$ kwContinue

-- TODO : Add For

stmtForP :: Parser Ast.Statement
stmtForP = undefined

-- ForStmt   = { "for" ~ ( ForClause | Condition )? ~ Block }
-- ForClause = { SimpleStmt? ~ ";" ~ Condition? ~ ";" ~ SimpleStmt? }
-- Condition = { expressionP }

-- TODO : Add support for more complex assignments

-- VarDecl     = { "var" ~ ( VarSpec | "(" ~ ( VarSpec ~ ";" )* ~ ")" ) }
-- VarSpec     = { IdentifierList ~ ( Type ~ ( "=" ~ ExpressionList )? | "=" ~ ExpressionList ) }

stmtVarDeclP :: Parser Ast.VarDecl
stmtVarDeclP = Ast.VarDecl <$ kwVar <*> choice' [listed stmtVarSpecP semicolon, (: []) <$> stmtVarSpecP]

stmtVarSpecP :: Parser Ast.VarSpec
stmtVarSpecP = Ast.VarSpec <$> identifierListP <*> optional' typeP <* symbol "=" <*> expressionListP

stmtIfElseP :: Parser Ast.IfElse
stmtIfElseP = do
  void kwIf
  stmt <- optional' $ simpleStmtP <* semicolon
  condition <- expressionP
  block <- stmtBlockP
  -- TODO : Add Else ("else" ~ ( IfElseStmt | Block ))?
  return $ Ast.IfElse {simpleStmt = stmt, condition = condition, block = block, elseStmt = Right []}

stmtBlockP :: Parser [Ast.Statement]
stmtBlockP = braces $ catMaybes <$> many (optional' statementP <* semicolon)

simpleStmtP :: Parser Ast.SimpleStmt
simpleStmtP = choice' [stmtAssignmentP, stmtIncP, stmtDecP, stmtShortVarDeclP, stmtExpressionP]

-- TODO : Add support for more complex assignments

stmtAssignmentP :: Parser Ast.SimpleStmt
stmtAssignmentP =
  Ast.StmtAssignment
    <$> do ids <- identifierListP; return $ Ast.AssignmentLhsVar <$> ids
    <* symbol "="
    <*> expressionListP

stmtIncP :: Parser Ast.SimpleStmt
stmtIncP = Ast.StmtInc <$> expressionP <* symbol "++"

stmtDecP :: Parser Ast.SimpleStmt
stmtDecP = Ast.StmtDec <$> expressionP <* symbol "--"

stmtShortVarDeclP :: Parser Ast.SimpleStmt
stmtShortVarDeclP = Ast.StmtShortVarDecl <$> identifierListP <* symbol ":=" <*> expressionListP

stmtExpressionP :: Parser Ast.SimpleStmt
stmtExpressionP = Ast.StmtExpression <$> expressionP

-- Array

arrayTypeP :: Parser Ast.ArrayType
arrayTypeP = do
  lenExpr <- brackets expressionP
  len <- do
    case simplifyConstExpr lenExpr of
      Just (Ast.LitInt len) -> return len
      _ -> fail "this is not a const int expression"
  t <- typeP
  return Ast.ArrayType {Ast.elementType = t, Ast.length = len}

-- Literal

literalP :: Parser Ast.Literal
literalP =
  choice'
    [ Ast.LitInt <$> intLitP,
      Ast.LitBool <$> boolLitP,
      Ast.LitString <$> stringLitP
      -- TODO : arrayLitP, functionLitP
    ]

-- Literals

-- TODO : Implement Function Literals

-- functionLitP   = { "func" ~ Signature ~ FunctionBody }

-- TODO : Implement Array Literals

arrayLitP :: Parser Ast.Literal
arrayLitP = do
  t <- arrayTypeP
  value <- arrayLitValueP
  return Ast.LitArray {t = t, value = value}

arrayLitValueP :: Parser [Ast.Element]
arrayLitValueP = undefined

-- ArrayLiteral      = { ArrayType ~ ArrayLiteralValue }
-- ArrayLiteralValue = { "{" ~ ( KeyedElementList ~ ","? )? ~ "}" }
-- KeyedElementList  = { KeyedElement ~ ( "," ~ KeyedElement )* }
-- KeyedElement      = { ( Key ~ ":" )? ~ Element }
-- Key               = { Expression }
-- Element           = { Expression | ArrayLiteralValue }

intLitP :: Parser Int
intLitP = fromIntegral <$> int

boolLitP :: Parser Bool
boolLitP = True <$ idTrue <|> False <$ idFalse

stringLitP :: Parser Text
stringLitP = lexeme $ Data.Text.concat <$> between (char '"') (char '"') (some stringChar)

-- Identifier

identifierListP :: Parser [Ast.Identifier]
identifierListP = sepBy identifierP comma

identifierP :: Parser Ast.Identifier
identifierP = lexeme $ Ast.Identifier <$> identifierTextP

stdlibFuncP :: Parser Ast.Identifier
stdlibFuncP = Ast.Identifier <$> stdlibFuncTextP

-- Utils

-- TODO : Use const expressions simplification

choice' :: (Foldable f, MonadParsec e s m, Functor f) => f (m a) -> m a
choice' ps = choice $ try <$> ps

optional' :: (MonadParsec e s m) => m a -> m (Maybe a)
optional' = optional . try