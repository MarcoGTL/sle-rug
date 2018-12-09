module Syntax

extend lang::std::Layout;
extend lang::std::Id;
import ParseTree;
import vis::Figure;
import vis::ParseTree;
import vis::Render;
/* render(visParsetree(parse(#Expr, "form test { question? } "))); */

/*
 * Concrete syntax of QL
 */

start syntax Form
  = "form" Id "{"Question*"}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  =  Str Id ":" Type
  | Str Id ":" Type "=" Expr
  | "{"Question*"}"
  | "if" "("Expr")" "{"Question*"}"
  | "if" "("Expr")" "{"Question*"}" "else" "{"Question*"}"
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = "("Expr")"
  | Id \ "true" \ "false" // true/false are reserved keywords.
  | Int
  | Bool
  | Str
  > "!" Expr
  > left (
     left Expr "*" Expr
    | left Expr "/" Expr
  )
  > left (
     left Expr "+" Expr
    | left Expr "-" Expr
  )
  > non-assoc ( Expr "\<" Expr
    | Expr "\<=" Expr
    | Expr "\>" Expr
    | Expr "\>=" Expr
  )
  > non-assoc ( Expr "==" Expr
    | Expr "!=" Expr
  )
  > non-assoc Expr "&&" Expr
  > non-assoc Expr "||" Expr
  ;
  
syntax Type
  = "boolean" | "integer" | "string";  

/* Or maybe something like (![\"] | "\\"<<"\"")* which accepts everything except another " unless it is \" */
lexical Str = "\""[A-Z a-z 0-9 \ !?/@#$%^&*()_+-=:;\<\>,.\\|]*"\""; 

lexical Int = [0-9]+;

lexical Bool =  "true" | "false";