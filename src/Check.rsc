module Check

import AST;
import Resolve;
import Message; // see standard library
import IO;
import Set;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];


// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv tenv = { <q.src, q.name, q.label, q.datatype> | /AQuestion q <- f.questions, (q has name)};
  return tenv;  
}

Type toType(AType t) {
  
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef)
  = ( {} | it + check(q, tenv, useDef) | /AQuestion q  <- f.questions );


// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef)
  = { error("Question has same name but different type.", q.src) | (q has name), size(tenv[_,q.name,_]) > 1}
  + { warning("Duplicate label encountered.", q.src) | (q has label), t := tenv<label,def>, size(t[q.label]) > 1 }
  + { error("Declared type does not match expression type.", q.src) | (q is computed), q.datatype != typeOf(q.expr,tenv,useDef)}
  + ( {} | it +  check(e, tenv, useDef) | (q has expr), /AExpr e <- q);

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};

  switch (e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };
	case not(AExpr expr, src = loc u):
	  msgs += { error("Expression is not a boolean", u) | typeOf(expr, tenv, useDef) != tbool()};
    case product(AExpr expr1, AExpr expr2):
    ;
    case quotient(AExpr expr1, AExpr expr2):
    ;
    case plus(AExpr expr1, AExpr expr2):
    ;
    case minus(AExpr expr1, AExpr expr2, src = loc u):
      msgs += { error("Trying to subract non-integers", u) | (typeOf(expr1, tenv, useDef) != tint())};

    case less(AExpr expr1, AExpr expr2):
    ;
    case lesseq(AExpr expr1, AExpr expr2):
    ;
    case greater(AExpr expr1, AExpr expr2):
    ;
    case greatereq(AExpr expr1, AExpr expr2):
    ;
    case equals(AExpr expr1, AExpr expr2):
    ;
    case notequals(AExpr expr1, AExpr expr2):
    ;
    case and(AExpr expr1, AExpr expr2):
    ;
    case or(AExpr expr1, AExpr expr2):
    ;
  }
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(str x, src = loc u):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    case integer(int x): return tint();
    case boolean(bool boolean): return tbool();
    case not(AExpr expr): return tbool();
    case product(AExpr expr1, AExpr expr2): return tint();
    case quotient(AExpr expr1, AExpr expr2): return tint();
    case plus(AExpr expr1, AExpr expr2): return tint();
    case minus(AExpr expr1, AExpr expr2): return tint();
    case less(AExpr expr1, AExpr expr2): return tbool();
    case lesseq(AExpr expr1, AExpr expr2): return tbool();
    case greater(AExpr expr1, AExpr expr2): return tbool();
    case greatereq(AExpr expr1, AExpr expr2): return tbool();
    case equals(AExpr expr1, AExpr expr2): return tbool();
    case notequals(AExpr expr1, AExpr expr2): return tbool();
    case and(AExpr expr1, AExpr expr2): return tbool();
    case or(AExpr expr1, AExpr expr2): return tbool();
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(str x, src = loc u), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 set[Message] thing(AForm f) {
  for (/AQuestion q <- f.questions, (q has name)) {
     if (q has expr) {
       println(q.expr);
       for (/AExpr e <- q.expr) {
  	     println(e);
  	   }
     }
     
  }
  return {};
}
 
 
