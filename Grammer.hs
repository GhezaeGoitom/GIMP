module Grammer where 

-- This module holds the Grammar of GIMP language.


-- basic types in GIMP Interpreters
data Type = 
          IntType Int     
        | BoolType Bool 
        | ArrayType [Int]
        deriving Show 

 
data AExp =              
          Constant Int
        | Avar String
        | ArrayVar String AExp   
        | Add AExp AExp
        | Sub AExp AExp
        | Mul AExp AExp
        | Div AExp AExp
        | Power AExp AExp
        deriving Show

data ArrayExp =                
         Array [AExp]
       | ArrayVariable String
       deriving Show


data BExp =                
          Boolean Bool
        | Bvar String
        | Lt AExp AExp
        | Gt AExp AExp
        | Eq AExp AExp
        | Neq AExp AExp
        | Lte AExp AExp
        | Gte AExp AExp
        | And BExp BExp
        | Or BExp BExp
        | Not BExp
        deriving Show

data Command =
        Skip
        | IfElse BExp [Command] [Command] 
	| Whiledo BExp [Command]
	| ArithAssign String AExp
        | BoolAssign String BExp  
        | ArrAssign String AExp AExp 
	| ArithDeclare String AExp
        | BoolDeclare String BExp
        | ArrDeclare String AExp 
        | ArrFullAssign String ArrayExp
	deriving Show

type Program = [Command] 