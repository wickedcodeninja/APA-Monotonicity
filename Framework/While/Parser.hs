-- |(C) Wout Elsinghorst 2013

-- |Skeleton stolen from http://www.haskell.org/haskellwiki/Parsing_a_simple_imperative_language
-- |Additional features by me.

module Framework.While.Parser
  ( parseProgram -- :: String -> Program Lab
  ) where
import Prelude hiding ( LT, GT, EQ, break )

import Text.ParserCombinators.Parsec hiding ( (<|>) )

import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Numbers

import qualified Text.Parsec.Token as Token

import Data.Functor
import Data.Functor.Identity
import Control.Applicative

import Framework.Information
import Framework.While.Language

import qualified Framework.Information.Labels as Labels 
  
  
pVar :: Parser String
pVar =  const "" <$> space
    <|> (:) <$> letter <*> pVar
   
pAExp :: Parser AExp
pAExp =  Num <$> parseIntegral
     <|> Var <$> pVar

languageDef :: GenLanguageDef String u Identity
languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "proc"
                                      , "call"
                                      , "while"
                                      , "do"
                                      , "break"
                                      , "continue"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      , "not"
                                      , "and"
                                      , "or"
                                      , "var"
                                      , "int"
                                      , "ret"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                      , "<", ">", "and", "or", "not"
                                      ]
            }
            
lexer = Token.makeTokenParser languageDef
       
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
comma      = Token.comma      lexer

statement :: Parser Statement'
statement =  parens statement
         <|> sequenceOfStatement
 
statement' :: Parser Statement'
statement' = (  ifStatement
            <|> whileStatement
            <|> skipStatement
            <|> assignStatement
            <|> callStatement
--            <|> (const break    <$> reserved "break"   )
--            <|> (const continue <$> reserved "continue") 
            <|> (declaration <$> decl) 
             )
 
sequenceOfStatement =
   do list <- (sepBy1 statement' semi)
      -- If there's only one statement return it without using Seq.
      return $ if length list == 1 then head list else Seq list
      
ifStatement :: Parser Statement'
ifStatement =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     reserved "{"
     stmt1 <- statement
     reserved "}"
     reserved "else"
     reserved "{"
     stmt2 <- statement
     reserved "}"
     return $ ifThenElse cond stmt1 stmt2
 
callStatement :: Parser Statement'
callStatement =
  do reserved "call"
     name <- identifier
     reserved "["
     argList <- sepBy aExpression comma
     reserved "]"
     return $ call name argList
 
whileStatement :: Parser Statement'
whileStatement =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     
     reserved "{"
     stmt <- statement
     reserved "}"
     return $ while cond stmt
 
assignStatement :: Parser Statement'
assignStatement =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ assign var expr
 
skipStatement :: Parser Statement'
skipStatement = reserved "skip" >> return skip

declType :: Parser DeclType
declType =  (const VarTy <$> reserved "var") 
        <|> (const ReturnTy <$> reserved "ret")

decl :: Parser Decl
decl = Decl <$> declType <*> identifier
  

type Function' = Function ()
  
function :: Parser Function'
function =
  do reserved "proc"
     name <- identifier
     reserved "["
     argList <- sepBy decl comma
     reserved "]"
     
     reserved "{"
     stmt <- statement
     reserved "}"
     return $ Function ((), ()) name argList stmt

program :: Parser Program'
program = 
  do functionList <- sepBy function whiteSpace
     stmt <- statement
     return $ Program functionList stmt
     
aExpression :: Parser AExp
aExpression = buildExpressionParser aOperators aTerm
 
bExpression :: Parser BExp
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Infix  (reservedOp "*"   >> return (AOperator Mul )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (AOperator Add )) AssocLeft]
             , [Infix  (reservedOp "-"   >> return (AOperator Sub )) AssocLeft]
             ]
 
bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (BOperator And   )) AssocLeft]
             , [Infix  (reservedOp "or"  >> return (BOperator Or    )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> Var <$> identifier
     <|> Num <$> integer
     
bTerm =  parens bExpression
     <|> (reserved "true"  >> return (Atom True ))
     <|> (reserved "false" >> return (Atom False))
     <|> rExpression
     
rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ ROperator op a1 a2
 
relation =   (reservedOp "<"  >> return LT)
         <|> (reservedOp ">"  >> return GT)
         <|> (reservedOp "==" >> return EQ)

type Statement' = Statement ()
type Program'   = Program   ()
        
        
parseProgram :: String -> Program Lab
parseProgram str =
  case parse (whiteSpace >> program) "" str of
    Left e  -> error $ show e
    Right r -> Labels.decorate r
 