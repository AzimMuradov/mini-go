{-# LANGUAGE DuplicateRecordFields #-}

module Analyzer.AnalyzedAst
  ( module Analyzer.AnalyzedAst,
    UnaryOp (..),
    BinaryOp (..),
    ForGoTo (..),
    IncDec (..),
  )
where

import Data.Text (Text)
import MaybeVoid (MaybeVoid)
import Parser.Ast (BinaryOp (..), ForGoTo (..), IncDec (..), UnaryOp (..))

--------------------------------------------------------Program---------------------------------------------------------

data Program = Program
  { topLevelVarDecls :: [VarDecl],
    topLevelFunctionDefs :: [FunctionDef]
  }
  deriving (Show)

data FunctionDef = FunctionDef {funcName :: Identifier, func :: Function}
  deriving (Show)

------------------------------------------------------Expressions-------------------------------------------------------

data Expression
  = ExprValue Value
  | ExprIdentifier Identifier
  | ExprUnaryOp UnaryOp Expression
  | ExprBinaryOp BinaryOp Expression Expression
  | ExprArrayAccessByIndex Expression Expression
  | ExprFuncCall Expression [Expression]
  deriving (Show)

-------------------------------------------------------Statements-------------------------------------------------------

data Statement
  = StmtReturn (MaybeVoid Expression)
  | StmtForGoTo ForGoTo
  | StmtFor For
  | StmtVarDecl VarDecl
  | StmtIfElse IfElse
  | StmtBlock Block
  | StmtSimple SimpleStmt
  deriving (Show)

type Block = [Statement]

data For = For {forHead :: ForHead, forBody :: Block}
  deriving (Show)

data ForHead = ForHead
  { forPreStmt :: Maybe SimpleStmt,
    forCondition :: Maybe Expression,
    forPostStmt :: Maybe SimpleStmt
  }
  deriving (Show)

data VarDecl = VarDecl {varName :: Identifier, varValue :: Expression}
  deriving (Show)

data IfElse = IfElse
  { ifPreStmt :: Maybe SimpleStmt,
    ifCondition :: Expression,
    ifBody :: Block,
    elseStmt :: Else
  }
  deriving (Show)

data Else = NoElse | Else Block | Elif IfElse
  deriving (Show)

data SimpleStmt
  = StmtAssignment Lvalue Expression
  | StmtIncDec Lvalue IncDec
  | StmtShortVarDecl Identifier Expression
  | StmtExpression Expression
  deriving (Show)

data Lvalue
  = LvalVar Identifier
  | LvalArrEl Identifier [Expression]
  deriving (Show)

---------------------------------------------------------Values---------------------------------------------------------

data Value
  = ValInt Int
  | ValBool Bool
  | ValString Text
  | ValArray [Expression]
  | ValFunction FunctionValue
  deriving (Show)

data FunctionValue
  = Function Function
  | Nil
  deriving (Show)

data Function
  = FuncOrdinary OrdinaryFunction
  | FuncStdLib Identifier
  deriving (Show)

data OrdinaryFunction = OrdinaryFunction {funcParams :: [Identifier], funcBody :: Block, funcVoidMark :: VoidMark}
  deriving (Show)

data VoidMark = VoidFunc | NonVoidFunc
  deriving (Show)

type Identifier = Text
