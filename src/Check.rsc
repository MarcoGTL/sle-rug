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
alias TEnv = rel[loc def, str name, str label, AType \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv tenv = { <q.src, q.name, q.label, q.datatype> | /AQuestion q := f, (q has name)};
  return tenv;  
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef)
  = ( {} | it + check(q, tenv, useDef) | /AQuestion q  := f);


// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) 
  = { error("Same name with different type", q.src) | (q has name), size(tenv<name,def,label,\type>[q.name]) > 1}
  + { warning("Same label is used multiple times", q.src) | (q has label), size(tenv<label,def,name,\type>[q.label]) > 1};
  

/*println(tenv);
  
  set[Message] msgs = {};  
  for (Rel r <- tenv) {
    
  }

if (q has name && size(tenv[_,_,q.name]) > 1) {
    msgs += { error("same name different type",q.src)};
  }
  if (q has label && size(tenv[_,q.label]) > 1) {
  	msgs += { warning("same name",q.src)};
  }

if (name == q.name) {
  	  if (q.datatype != q2.datatype) {
  	    msgs += { error("same name different type",q.src)};
  	  } else {
   	    msgs += { warning("same name",q.src)};
  	  }
  	}
  	
  	 println(msgs);
 println("\n");
 return msgs;
}
*/ 



// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };

    // etc.
  }
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(str x, src = loc u):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    // etc.
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
 
 

