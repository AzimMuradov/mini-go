{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Contains all AST elements, all of these produced by the [Parser]("Parser.Parser") module.
module Parser.Ast where

import Data.Text (Text)

--------------------------------------------------------Program---------------------------------------------------------

-- * Program

-- | The head of the AST.
data Program = Program
  { -- | Top level variable declarations.
    topLevelVarDecls :: [VarDecl],
    -- | Top level function definitions.
    topLevelFunctionDefs :: [FunctionDef]
  }
  deriving (Show)

-- | Function definition.
data FunctionDef = FunctionDef {funcName :: Identifier, func :: Function}
  deriving (Show)

------------------------------------------------------Expressions-------------------------------------------------------

-- * Expressions

-- | Expression.
data Expression
  = -- | Value expression.
    ExprValue Value
  | -- | Identifier expression.
    ExprIdentifier Identifier
  | -- | Unary operation expression (e.g., @!x@, @-4@).
    ExprUnaryOp UnaryOp Expression
  | -- | Binary operation expression (e.g., @x + 7@).
    ExprBinaryOp BinaryOp Expression Expression
  | -- | Array access by index expression.
    --
    -- > a[3]
    --
    -- > // func foo() int
    -- > ([2] int {3, 5})[1 + foo()]
    ExprArrayAccessByIndex Expression Expression
  | -- | Function call expression.
    --
    -- > foo(17, x, bar())
    --
    -- > (func (x int) int { return x * x; })(3)
    ExprFuncCall Expression [Expression]
  | -- | @len@ function call expression.
    --
    -- > len("abcd") // returns 4
    --
    -- > len([100] int {}) // returns 100
    ExprLenFuncCall Expression
  | -- | @println@ function call expression.
    --
    -- > println("some logs...") // prints "some logs...\n"
    --
    -- > println(3 + 6)  // prints "9\n"
    ExprPrintlnFuncCall Expression
  | -- | @panic@ function call expression.
    --
    -- > panic("ERROR!!!") // fails with: "panic: ERROR!!!\n"
    ExprPanicFuncCall Expression
  deriving (Show)

-- ** Operators

-- | Binary operators.
data BinaryOp
  = -- | Or operator (@a || b@), works only for @bool@.
    OrOp
  | -- | And operator (@a && b@), works only for @bool@.
    AndOp
  | -- | Equality operator (@a == b@).
    EqOp
  | -- | Inequality operator (@a != b@).
    NeOp
  | -- | Less than or equal operator (@a <= b@), works only for @int@ and @string@.
    LeOp
  | -- | Less than operator (@a < b@), works only for @int@ and @string@.
    LtOp
  | -- | More than or equal operator (@a >= b@), works only for @int@ and @string@.
    MeOp
  | -- | More than operator (@a > b@), works only for @int@ and @string@.
    MtOp
  | -- | Plus operator (@a + b@), works only for @int@ and @string@.
    PlusOp
  | -- | Minus operator (@a - b@), works only for @int@.
    MinusOp
  | -- | Multiply operator (@a * b@), works only for @int@.
    MultOp
  | -- | Divide operator (@a / b@), works only for @int@.
    DivOp
  | -- | Module operator (@a % b@), works only for @int@.
    ModOp
  deriving (Show)

-- | Unary operators.
data UnaryOp
  = -- | Unary plus operator (@+a@), works only for @int@.
    UnaryPlusOp
  | -- | Unary minus operator (@-a@), works only for @int@.
    UnaryMinusOp
  | -- | Not operator (@!a@), works only for @bool@.
    NotOp
  deriving (Show)

---------------------------------------------------------Types----------------------------------------------------------

-- * Types

-- | All existing types.
data Type
  = -- | 32-bit/64-bit (depending on the machine) integer type.
    TInt
  | -- | Boolean type.
    TBool
  | -- | String type.
    TString
  | -- | Array type.
    TArray ArrayType
  | -- | Function type.
    TFunction FunctionType
  deriving (Show)

-- | Array type, it contains the length of the array and its elements type.
--
-- > [3 + 4] int
data ArrayType = ArrayType {elementType :: Type, length :: Expression}
  deriving (Show)

-- | Function type,
-- it contains the result of the function (which can be @void@ if the result is equal to 'Nothing')
-- and its parameters types.
--
-- > func (int, string) bool
data FunctionType = FunctionType {parameters :: [Type], result :: Maybe Type}
  deriving (Show)

-------------------------------------------------------Statements-------------------------------------------------------

-- * Statements

-- | Statement.
data Statement
  = -- | Return statement with optional return value (in the case of 'Nothing' we assume, that it is @void@).
    StmtReturn (Maybe Expression)
  | -- | Break statement, should be inside @for@.
    StmtBreak
  | -- | Continue statement, should be inside @for@.
    StmtContinue
  | -- | For statement.
    StmtFor For
  | -- | Var declaration statement.
    StmtVarDecl VarDecl
  | -- | If-else statement.
    StmtIfElse IfElse
  | -- | Block statement.
    --
    -- > { 34; foo(34); if true {} else {}; return 17; }
    StmtBlock [Statement]
  | -- | Simple statement.
    StmtSimple SimpleStmt
  deriving (Show)

-- | For statement, can represent any of the 3 possible @for@ kinds.
data For = For {kind :: ForKind, block :: [Statement]}
  deriving (Show)

-- | For statement, can represent any of the 3 possible @for@ kinds.
data ForKind
  = -- | For kind, represents classic for loop.
    --
    -- > for i := 0; i < n; i++ {
    -- >   foo(i * i);
    -- > }
    ForKindFor {preStmt :: Maybe SimpleStmt, condition :: Maybe Expression, postStmt :: Maybe SimpleStmt}
  | -- | While kind, represents classic while loop.
    --
    -- > for i < n {
    -- >   foo(i * i);
    -- >   i = i + 2;
    -- > }
    ForKindWhile {whileCondition :: Expression}
  | -- | Loop kind, represents endless loop (while true).
    --
    -- > for {
    -- >   temp := foo(i * i * i);
    -- >   if temp == 108 { break; }
    -- >   temp = temp + 23;
    -- > }
    ForKindLoop
  deriving (Show)

-- | Var declaration.
--
-- > var x int = 3
--
-- > var y = "hello"
--
-- > var z int
data VarDecl
  = VarDecl Identifier (Maybe Type) Expression
  | DefaultedVarDecl Identifier Type
  deriving (Show)

-- | If-else statement.
--
-- > if i < 42 { return "hello"; } else { return "goodbye"; }
--
-- > if true { println("hello"); }
data IfElse = IfElse
  { condition :: Expression,
    block :: [Statement],
    elseStmt :: Maybe (Either IfElse [Statement])
  }
  deriving (Show)

-- | Simple statement, its main difference between other statements is that it can be used inside @for@ \"pre\" and \"post\" statements.
--
-- > for i := 0; i < n; i++ { println(i); }
data SimpleStmt
  = -- | Assignment statement (e.g., @x = 17@, @a[3] = \"42\"@).
    StmtAssignment UpdatableElement Expression
  | -- | Increment statement (e.g., @x++@, @a[3]++@).
    StmtInc UpdatableElement
  | -- | Decrement statement (e.g., @x--@, @a[3]--@).
    StmtDec UpdatableElement
  | -- | Short var declaration statement (e.g., @x := 3@, @y := true@).
    StmtShortVarDecl Identifier Expression
  | -- | Expression statement.
    StmtExpression Expression
  deriving (Show)

-- | Any element that can be updated.
data UpdatableElement
  = -- | Any variable can be updated (e.g., @x = 3@, @x++@).
    UpdVar Identifier
  | -- | Any array element can be updated (e.g., @a[5][7] = 3@, @a[0]++@).
    UpdArrEl Identifier [Expression]
  deriving (Show)

---------------------------------------------------------Values---------------------------------------------------------

-- * Values

-- | Literal, array, or function value.
data Value
  = -- | Int literal value (e.g., @17@, @0xFF@, @0b101001@).
    ValInt Integer
  | -- | Boolean literal value (e.g., @true@, @false@).
    ValBool Bool
  | -- | String literal value (e.g., @\"Hello\"@, @\"\"@, @\"Some\\ntext\"@).
    ValString Text
  | -- | Array value.
    ValArray ArrayValue
  | -- | Function value.
    ValFunction FunctionValue
  deriving (Show)

-- | Array value.
--
-- > [3] int {1, 2}
--
-- > [10] bool {}
--
-- > // same as [3][2] int {{1, 0}, {0, 0}, {1, 2}}
-- > [3][2] int {{1}, 2 : {1, 2}}
data ArrayValue = ArrayValue {t :: ArrayType, elements :: [KeyedElement]}
  deriving (Show)

-- | Array's optionally keyed element.
--
-- > 14
--
-- > 2 : 14
--
-- > 1 : {"abc", 2 : "xyz"}
data KeyedElement = KeyedElement {key :: Maybe Expression, element :: Element}
  deriving (Show)

-- | Array's element.
--
-- > 14
--
-- > {"abc", 3 : "xyz"}
data Element = Element Expression | ElementList [KeyedElement]
  deriving (Show)

-- | Function value.
data FunctionValue
  = -- | Anonymous function.
    --
    -- > func (x int) int { return x * x; }
    --
    -- > func () {}
    AnonymousFunction Function
  | -- | Null literal (@nil@).
    Nil
  deriving (Show)

-- | Function representation without name.
data Function = Function {signature :: FunctionSignature, body :: [Statement]}
  deriving (Show)

-- | Function signature,
-- it contains the result of the function (which can be @void@ if the result is equal to 'Nothing')
-- and its parameters.
data FunctionSignature = FunctionSignature {parameters :: [(Identifier, Type)], result :: Maybe Type}
  deriving (Show)

-- | Any valid identifier (e.g., @he42llo@, @_42@).
type Identifier = Text