{-# LANGUAGE FlexibleInstances #-}

-- |(C) Wout Elsinghorst 2014

module Framework.While.Language
  ( AOperator (..), AExp (..)
  , BOperator (..), BExp (..)
  , ROperator (..)
  , Var  (..)
  , Statement (..)
  , Function  (..)
  , Program   (..)
  , Decl      (..)
  , DeclType  (..)
  
  , declaration
  , assign
  , skip
  , ifThenElse
  , while
  , call
--  , break
--  , continue

  ) where

import Prelude hiding ( break, EQ, LT, GT )
  
import Data.List
  
type Var = String
  
data AOperator = Add | Sub | Mul
  deriving (Ord, Eq)
 
instance Show AOperator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  
data BOperator = And | Or
  deriving (Ord, Eq)
  
instance Show BOperator where
  show And = "&&"
  show Or = "||"
  
data ROperator = LT | GT | EQ
  deriving (Ord, Eq)

instance Show ROperator where
  show LT = "<"
  show GT = ">"
  show EQ = "=="

data AExp = Num Integer
          | Var Var
          | AOperator AOperator AExp AExp
  deriving (Ord, Eq)
          
instance Show AExp where
  show (Num n) = show n
  show (Var v) = v
  show (AOperator op l r) = show l ++ " " ++ show op ++ " " ++ show r 
          
data BExp = Atom Bool 
          | Not BExp 
          | BOperator BOperator BExp BExp
          | ROperator ROperator AExp AExp
  deriving (Ord, Eq)
  
instance Show BExp where
  show (Atom True) = "true"
  show (Atom False) = "false"
  show (BOperator op l r) = show l ++ " " ++ show op ++ " " ++ show r 
  show (ROperator op l r) = show l ++ " " ++ show op ++ " " ++ show r 
  
data DeclType = VarTy | ReturnTy
  deriving (Ord, Eq)
  
instance Show DeclType where
  show VarTy = "var"
  show ReturnTy = "ret"
  
data Decl = Decl DeclType Var
  deriving (Ord, Eq)

instance Show Decl where
  show (Decl ty var) = show ty ++ " " ++ var
  
data Program lab = Program [Function lab] (Statement lab)
 
instance Show (Program Int) where
  show (Program fs stmt) = concat (intersperse "\n\n" (map show fs)) ++ "\n" ++ show stmt

data Function lab = Function (lab, lab) String [Decl] (Statement lab)


instance Show (Function Int) where
  show (Function (e, l) nm decls stmt) = nm ++ "[" ++ show e ++ ", " ++ show l ++ "] (" ++ printedDecls ++ ") {\n" ++ show stmt ++ "\n}\n" where
    printedDecls = concat (intersperse ", " . map show $ decls)

  
data Statement lab
          = Seq [Statement lab]
          | Declaration lab Decl
          | Assign      lab Var AExp
          | Skip        lab
          | IfThenElse  lab BExp (Statement lab) (Statement lab) 
          | While       lab BExp (Statement lab)
          | Call        (lab, lab) String [AExp]
--          | Break       lab
--          | Continue    lab


instance Show (Statement Int) where
  show (Seq stmts) = "  " ++ concat (intersperse "\n  " . map show $ stmts)
  show (Declaration lab dec) = show dec ++ "[" ++ show lab ++ "]" ++ ";"
  show (Assign lab var ae) = var ++ "[" ++ show lab ++ "] := " ++ show ae ++ ";"
  show (Skip lab) = "skip[" ++ show lab ++ "];"
  show (IfThenElse lab be sta stb) = "if (" ++ show be ++ ") [" ++ show lab ++ "] {\n" ++ show sta ++ "\n} else {" ++ show stb ++ "\n}"
  show (While lab be stmt) = "while [" ++ show lab ++ "] (" ++ show be ++ ") {\n" ++ show stmt ++ "\n}" 
  show (Call (c, r) nm aes) = "call [" ++ show c ++ ", " ++ show r ++ "] (" ++ concat (intersperse ", " . map show $ aes) ++ ");"
  
declaration = Declaration ()
assign = Assign ()
skip   = Skip ()
ifThenElse = IfThenElse ()
while = While ()
call = Call ((), ())
--break = Break ()
--continue = Continue ()

