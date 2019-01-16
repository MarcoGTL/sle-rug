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
  TEnv tenv = { <q.nsrc, q.name, q.label, toType(q.datatype)> | /AQuestion q <- f.questions, (q has name)};
  return tenv;  
}

Type toType(AType at) {
  switch(at) {
    case atint(): return tint();
    case atbool(): return tbool();
  	case atstr(): return tstr();
  	default: return tunknown();
  }
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef)
  = ( {} | it + check(q, tenv, useDef) | /AQuestion q  <- f.questions );

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef)
  = { error("Question has same name but different type.", q.nsrc) | (q has name), size(tenv[_,q.name,_]) > 1}
  + { warning("Duplicate label encountered.", q.lsrc) | (q has label), t := tenv<label,def>, size(t[q.label]) > 1 }
  + { error("Declared type is of type <toType(q.datatype)> but expression is <typeOf(q.expr,tenv,useDef)>", q.tsrc) | (q is computed), toType(q.datatype) != typeOf(q.expr,tenv,useDef)}
  + { error("Expected a boolean but expression is of type <typeOf(q.condition,tenv,useDef)>", q.condition.src) | (q is ifthen || q is ifthenelse), typeOf(q.condition,tenv,useDef) != tbool()}
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
	  msgs += { error("Expression in negation is not a boolean", u) | typeOf(expr, tenv, useDef) != tbool()};
    case product(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in multiplication is not an integer", u) | (typeOf(expr1, tenv, useDef) != tint())}
            + {error("Second expression in multiplication is not an integer", u) | (typeOf(expr2, tenv, useDef) != tint())};
    case quotient(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in division is not an integer", u) | (typeOf(expr1, tenv, useDef) != tint())}
            + {error("Second expression in division is not an integer", u) | (typeOf(expr2, tenv, useDef) != tint())};
    case plus(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in addition is not an integer", u) | (typeOf(expr1, tenv, useDef) != tint())}
            + {error("Second expression in addition is not an integer", u) | (typeOf(expr2, tenv, useDef) != tint())};
    case minus(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in subtraction is not an integer", u) | (typeOf(expr1, tenv, useDef) != tint())}
            + {error("Second expression in subtraction is not an integer", u) | (typeOf(expr2, tenv, useDef) != tint())};
    case less(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in comparison is not an integer", u) | (typeOf(expr1, tenv, useDef) != tint())}
            + {error("Second expression in comparison is not an integer", u) | (typeOf(expr2, tenv, useDef) != tint())};
    case lesseq(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in comparison is not an integer", u) | (typeOf(expr1, tenv, useDef) != tint())}
            + {error("Second expression in comparison is not an integer", u) | (typeOf(expr2, tenv, useDef) != tint())};
    case greater(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in comparison is not an integer", u) | (typeOf(expr1, tenv, useDef) != tint())}
            + {error("Second expression in comparison is not an integer", u) | (typeOf(expr2, tenv, useDef) != tint())};
    case greatereq(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in comparison is not an integer", u) | (typeOf(expr1, tenv, useDef) != tint())}
            + {error("Second expression in comparison is not an integer", u) | (typeOf(expr2, tenv, useDef) != tint())};
    case equals(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("Comparing expressions of different types", u) | (typeOf(expr1, tenv, useDef) != typeOf(expr2, tenv, useDef))};
    case notequals(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("Comparing expressions of different types", u) | (typeOf(expr1, tenv, useDef) != typeOf(expr2, tenv, useDef))};
    case and(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in and operation is not a boolean", u) | (typeOf(expr1, tenv, useDef) != tbool())}
            + {error("Second expression in and operation is not a boolean", u) | (typeOf(expr2, tenv, useDef) != tbool())};
    case or(AExpr expr1, AExpr expr2, src = loc u):
      msgs += {error("First expression in or operation is not a boolean", u) | (typeOf(expr1, tenv, useDef) != tbool())}
            + {error("Second expression in or operation is not a boolean", u) | (typeOf(expr2, tenv, useDef) != tbool())};
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
    case string(str string): return tstr();
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
    default: return tunknown();
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
 
 
