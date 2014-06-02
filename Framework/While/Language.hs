-- |(C) Wout Elsinghorst 2013

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

import Prelude hiding ( break )
  
type Var = String
  
data AOperator = Add | Sub | Mul
  deriving (Show, Ord, Eq)
data BOperator = And | Or
  deriving (Show, Ord, Eq)
data ROperator = LT | GT | EQ
  deriving (Show, Ord, Eq)


data AExp = Num Integer
          | Var Var
          | AOperator AOperator AExp AExp
  deriving (Show, Ord, Eq)
          
data BExp = Atom Bool 
          | Not BExp 
          | BOperator BOperator BExp BExp
          | ROperator ROperator AExp AExp
  deriving (Show, Ord, Eq)

data DeclType = VarTy | ReturnTy
  deriving (Show, Ord, Eq)
data Decl = Decl DeclType Var
  deriving (Show, Ord, Eq)

data Program lab = Program [Function lab] (Statement lab)
  deriving Show
  
data Function lab = Function (lab, lab) String [Decl] (Statement lab)
  deriving Show

  
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
  deriving Show

  
declaration = Declaration ()
assign = Assign ()
skip   = Skip ()
ifThenElse = IfThenElse ()
while = While ()
call = Call ((), ())
--break = Break ()
--continue = Continue ()

