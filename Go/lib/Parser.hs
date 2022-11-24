{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (parse) where

import qualified Ast
import AstOptimizer (simplifyConstExpr)
import Control.Applicative.Combinators (between)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Either (lefts, rights)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, concat)
import Lexer
import Text.Megaparsec (MonadParsec (..), choice, eitherP, many, optional, parseMaybe, (<|>))
import Text.Megaparsec.Char (char)

---------------------------------------------------------Parser---------------------------------------------------------

-- | Parser entry point
parse :: Text -> Maybe Ast.Program
parse = parseMaybe $ sc *> programP <* eof

--------------------------------------------------------Program---------------------------------------------------------

-- | Program parser.
programP :: Parser Ast.Program
programP = do
  decls <- many topLevelDeclP
  return Ast.Program {topLevelVarDecls = lefts decls, topLevelFunctionDefs = rights decls}

-- | Top level declaration parser, it parses either var declaration or function definition.
topLevelDeclP :: Parser (Either Ast.VarDecl Ast.FunctionDef)
topLevelDeclP = eitherP (try (stmtVarDeclP <* semicolon)) (try functionDefP)

-- | Function definition parser.
functionDefP :: Parser Ast.FunctionDef
functionDefP = Ast.FunctionDef <$ kwFunc <*> identifierP <*> functionLitWithoutKwP

------------------------------------------------------Expressions-------------------------------------------------------

-- | Expression parser.
expressionP :: Parser Ast.Expression
expressionP = makeExprParser termExpressionP opsTable

-- | Terminal expression parser, its terminal in terms of `makeExprParser` parser.
termExpressionP :: Parser Ast.Expression
termExpressionP = choice' [parens expressionP, Ast.ExprLiteral <$> literalP, Ast.ExprIdentifier <$> identifierP]

-- TODO : Support all operators

-- | Operators table, contains all operator parsers and their fixity.
opsTable :: [[Operator Parser Ast.Expression]]
opsTable =
  [ [funcCallOp, arrayAccessByIndexOp],
    [ unaryOp "+" Ast.UnaryPlusOp,
      unaryOp "-" Ast.UnaryMinusOp,
      unaryOp "!" Ast.NotOp,
      unaryOp "^" Ast.BitwiseComplementOp
    ],
    [ mulOp "*" Ast.MultOp
    -- mulOp "/" Ast.DivOp,
    -- mulOp "%" Ast.ModOp,
    -- mulOp "<<" Ast.BitShiftLeftOp,
    -- mulOp ">>" Ast.BitShiftRightOp,
    -- mulOp "&^" Ast.BitClearOp,
    -- mulOp "&" Ast.BitAndOp
    ],
    [ addOp "+" Ast.PlusOp,
      addOp "-" Ast.MinusOp
      -- addOp "|" Ast.BitOrOp,
      -- addOp "^" Ast.BitXorOp
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

-- Associativity and arity types

-- | Utility function, that takes operator symbol, operator function and gives new binary operator in return.
binary :: Text -> (Ast.Expression -> Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
binary opSym fun = InfixL $ fun <$ symbol opSym

-- | Utility function, that takes operator symbol, operator function and gives new prefix operator in return.
prefix :: Text -> (Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
prefix opSym fun = Prefix $ fun <$ symbol opSym

-- | Utility function, that takes operator symbol, operator function and gives new postfix operator in return.
postfix :: Parser (Ast.Expression -> Ast.Expression) -> Operator Parser Ast.Expression
postfix = Postfix

-- Operators types

andOrOp :: Text -> Ast.BinaryOp -> Operator Parser Ast.Expression
andOrOp opSym op = binary opSym (Ast.ExprBinaryOp op)

relOp :: Text -> Ast.RelOp -> Operator Parser Ast.Expression
relOp opSym op = binary opSym (Ast.ExprBinaryOp (Ast.RelOp op))

addOp :: Text -> Ast.AddOp -> Operator Parser Ast.Expression
addOp opSym op = binary opSym (Ast.ExprBinaryOp (Ast.AddOp op))

mulOp :: Text -> Ast.MulOp -> Operator Parser Ast.Expression
mulOp opSym op = binary opSym (Ast.ExprBinaryOp (Ast.MulOp op))

unaryOp :: Text -> Ast.UnaryOp -> Operator Parser Ast.Expression
unaryOp opSym op = prefix opSym (Ast.ExprUnaryOp op)

funcCallOp :: Operator Parser Ast.Expression
funcCallOp = postfix $ flip Ast.ExprFuncCall <$> listed expressionP comma

-- TODO : implement `arrayAccessByIndexOp`

arrayAccessByIndexOp :: Operator Parser Ast.Expression
arrayAccessByIndexOp = postfix $ do
  indexExpr <- brackets expressionP
  index <- do
    case simplifyConstExpr indexExpr of
      Just (Ast.LitInt len) -> return len
      _ -> fail "this is not a const int expression"
  return $ \arr -> Ast.ExprArrayAccessByIndex arr index

---------------------------------------------------------Types----------------------------------------------------------

-- | Type parser.
typeP :: Parser Ast.Type
typeP =
  choice'
    [ Ast.TInt <$ idInt,
      Ast.TBool <$ idBool,
      Ast.TString <$ idString,
      functionTypeP,
      parens typeP
    ]

-- TODO : arrayTypeP,

-- arrayTypeP :: Parser Ast.ArrayType
-- arrayTypeP = do
--   lenExpr <- brackets expressionP
--   len <- do
--     case simplifyConstExpr lenExpr of
--       Just (Ast.LitInt len) -> return len
--       _ -> fail "this is not a const int expression"
--   t <- typeP
--   return Ast.ArrayType {Ast.elementType = t, Ast.length = len}

-- | Function type (e.g., `func (int, string) bool`) parser.
functionTypeP :: Parser Ast.Type
functionTypeP = do
  void kwFunc
  params <- listed typeP comma
  result <- optional' typeP
  return $ Ast.TFunction $ Ast.FunctionType params result

-------------------------------------------------------Statements-------------------------------------------------------

-- | Statement parser.
statementP :: Parser Ast.Statement
statementP =
  choice'
    [ stmtReturnP,
      stmtBreakP,
      stmtContinueP,
      Ast.StmtVarDecl <$> stmtVarDeclP,
      Ast.StmtIfElse <$> stmtIfElseP,
      Ast.StmtBlock <$> stmtBlockP,
      Ast.StmtSimple <$> simpleStmtP
    ]

-- | Return statement parser.
stmtReturnP :: Parser Ast.Statement
stmtReturnP = Ast.StmtReturn <$ kwReturn <*> optional' expressionP

-- | Break statement parser.
stmtBreakP :: Parser Ast.Statement
stmtBreakP = Ast.StmtBreak <$ kwBreak

-- | Continue statement parser.
stmtContinueP :: Parser Ast.Statement
stmtContinueP = Ast.StmtContinue <$ kwContinue

-- TODO : stmtForP

-- stmtForP :: Parser Ast.Statement
-- stmtForP = todo $ unpack "`for` statement parser"

-- ForStmt   = { "for" ~ ( ForClause | Condition )? ~ Block }
-- ForClause = { SimpleStmt? ~ ";" ~ Condition? ~ ";" ~ SimpleStmt? }
-- Condition = { expressionP }

-- | Var declaration statement parser.
stmtVarDeclP :: Parser Ast.VarDecl
stmtVarDeclP = Ast.VarDecl <$ kwVar <*> choice' [listed1 varSpecP semicolon, (: []) <$> varSpecP]

-- | Var specification parser.
varSpecP :: Parser Ast.VarSpec
varSpecP = do
  name <- identifierP
  (t, expr) <-
    choice'
      [ do
          t <- typeP
          void $ symbol "="
          expr <- fromMaybe (defaultValue t) <$> optional' expressionP
          return (Just t, expr),
        do
          t <- optional' typeP
          void $ symbol "="
          expr <- expressionP
          return (t, expr)
      ]
  return $ Ast.VarSpec name t expr
  where
    defaultValue :: Ast.Type -> Ast.Expression
    defaultValue t = Ast.ExprLiteral $ case t of
      Ast.TInt -> Ast.LitInt 0
      Ast.TBool -> Ast.LitBool False
      Ast.TString -> Ast.LitString ""
      Ast.TArray arrT@(Ast.ArrayType elT len) ->
        Ast.LitArray (Ast.ArrayLiteral arrT $ replicate len (Ast.ElementExpr $ defaultValue elT))
      Ast.TFunction _ -> Ast.LitFunction Ast.Nil

-- If-else statement parser.
stmtIfElseP :: Parser Ast.IfElse
stmtIfElseP = do
  void kwIf
  stmt <- optional' $ simpleStmtP <* semicolon
  condition <- expressionP
  block <- stmtBlockP
  -- TODO : Add Else ("else" ~ ( IfElseStmt | Block ))?
  return $ Ast.IfElse {simpleStmt = stmt, condition = condition, block = block, elseStmt = Right []}

-- | Block statement parser.
stmtBlockP :: Parser [Ast.Statement]
stmtBlockP = braces $ catMaybes <$> many (optional' statementP <* semicolon)

-- | Simple statement parser.
simpleStmtP :: Parser Ast.SimpleStmt
simpleStmtP = choice' [stmtAssignmentP, stmtIncP, stmtDecP, stmtShortVarDeclP, stmtExpressionP]

-- TODO : Add support for `Ast.UpdArrEl`

-- | Assignment statement parser.
stmtAssignmentP :: Parser Ast.SimpleStmt
stmtAssignmentP = Ast.StmtAssignment . Ast.UpdVar <$> identifierP <* symbol "=" <*> expressionP

-- | Increment statement parser.
stmtIncP :: Parser Ast.SimpleStmt
stmtIncP = Ast.StmtInc . Ast.UpdVar <$> identifierP <* symbol "++"

-- | Decrement statement parser.
stmtDecP :: Parser Ast.SimpleStmt
stmtDecP = Ast.StmtDec . Ast.UpdVar <$> identifierP <* symbol "--"

-- | Short var declaration statement parser.
stmtShortVarDeclP :: Parser Ast.SimpleStmt
stmtShortVarDeclP = Ast.StmtShortVarDecl <$> identifierP <* symbol ":=" <*> expressionP

-- | Expression statement parser.
stmtExpressionP :: Parser Ast.SimpleStmt
stmtExpressionP = Ast.StmtExpression <$> expressionP

--------------------------------------------------------Literals--------------------------------------------------------

-- | Literal parser.
literalP :: Parser Ast.Literal
literalP =
  choice'
    [ Ast.LitInt <$> intLitP,
      Ast.LitBool <$> boolLitP,
      Ast.LitString <$> stringLitP,
      Ast.LitFunction <$> functionLitP
    ]

-- TODO : arrayLitP

-- arrayLitP :: Parser Ast.Literal
-- arrayLitP = do
--   t <- arrayTypeP
--   value <- arrayLitValueP
--   return $ Ast.LitArray Ast.ArrayLiteral {t = t, value = value}

-- arrayLitValueP :: Parser [Ast.Element]
-- arrayLitValueP = todo $ unpack "array literal value parser"

-- ArrayLiteral      = { ArrayType ~ ArrayLiteralValue }
-- ArrayLiteralValue = { "{" ~ ( KeyedElementList ~ ","? )? ~ "}" }
-- KeyedElementList  = { KeyedElement ~ ( "," ~ KeyedElement )* }
-- KeyedElement      = { ( Key ~ ":" )? ~ Element }
-- Key               = { Expression }
-- Element           = { Expression | ArrayLiteralValue }

-- | Function literal (can also be nil) parser.
functionLitP :: Parser Ast.FunctionLiteral
functionLitP = choice' [Ast.Nil <$ idNil, kwFunc *> functionLitWithoutKwP]

-- | Function literal without `func` keyword (e.g., `(x int, y string) bool { return; }`) parser.
functionLitWithoutKwP :: Parser Ast.FunctionLiteral
functionLitWithoutKwP = Ast.FunctionLiteral <$> functionSignatureP <*> stmtBlockP

-- | Function signature (e.g., `(x int, y string) bool`) parser.
functionSignatureP :: Parser Ast.FunctionSignature
functionSignatureP = do
  params <- listed ((,) <$> identifierP <*> typeP) comma
  result <- optional' typeP
  return $ Ast.FunctionSignature params result

-- TODO : Use const expressions simplification (see `AstOptimizer` module)

-- | Integer literal parser.
intLitP :: Parser Int
intLitP = fromIntegral <$> int

-- | Boolean literal parser.
boolLitP :: Parser Bool
boolLitP = True <$ idTrue <|> False <$ idFalse

-- | String literal parser.
stringLitP :: Parser Text
stringLitP = lexeme $ Data.Text.concat <$> between (char '"') (char '"') (many stringChar)

---------------------------------------------------------Utils----------------------------------------------------------

-- | Choice between elements parser with built-in backtracking support.
choice' :: (Foldable f, MonadParsec e s m, Functor f) => f (m a) -> m a
choice' ps = choice $ try <$> ps

-- | Optional element parser with built-in backtracking support.
optional' :: (MonadParsec e s m) => m a -> m (Maybe a)
optional' = optional . try