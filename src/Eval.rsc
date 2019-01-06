module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
 VEnv venv = ( q.name : vint(0)  | /AQuestion q <- f.questions, (q has datatype), (q.datatype ==  tint()));
 venv += ( q.name : vbool(false)  | /AQuestion q <- f.questions, (q has datatype), (q.datatype ==  tbool()));
 venv += ( q.name : vstr("")  | /AQuestion q <- f.questions, (q has datatype), (q.datatype ==  tstr()));
  return venv;
}


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  return (); 
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  return (); 
}



Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(str x): return venv[x];
    case integer(int x): return vint(x);
    case boolean(bool boolean): return vbool(boolean);
    case string(str string): return vstr(string);
    case not(AExpr expr): 
      switch(eval(expr, venv)) {
    	case vbool(x): return vbool(!x);
    	default: throw "Cannot negate <expr>";
      }
    case product(AExpr expr1, AExpr expr2):
	  switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vint(x * y);
	    default: throw "Cannot multiply <expr1> by <expr2>";
	  }
    case quotient(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vint(x / y);
	    default: throw "Cannot divide <expr1> by <expr2>";
	  }
    case plus(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vint(x + y);
	    default: throw "Cannot add <expr1> to <expr2>";
	  }
    case minus(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vint(x - y);
	    default: throw "Cannot subtract <expr1> from <expr2>";
	  }
    case less(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x < y);
	    default: throw "Cannot compare <expr1> with <expr2>";
	  }
    case lesseq(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x <= y);
	    default: throw "Cannot compare <expr1> with <expr2>";
	  }
    case greater(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x > y);
	    default: throw "Cannot compare <expr1> with <expr2>";
	  }
    case greatereq(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x >= y);
	    default: throw "Cannot compare <expr1> with <expr2>";
	  }
    case equals(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x == y);
	    case [vbool(x), vbool(y)]: return vbool(x == y);
	    case [vstr(x), vstr(y)]: return vbool(x == y);
	    default: throw "Cannot compare <expr1> with <expr2>";
	  }
    case notequals(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x != y);
	    case [vbool(x), vbool(y)]: return vbool(x != y);
	    case [vstr(x), vstr(y)]: return vbool(x != y);
	    default: throw "Cannot compare <expr1> with <expr2>";
	  }
    case and(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vbool(x), vbool(y)]: return vbool(x && y);
	    default: throw "Cannot compare <expr1> with <expr2>";
	  }
    case or(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vbool(x), vbool(y)]: return vbool(x || y);
	    default: throw "Cannot compare <expr1> with <expr2>";
	  }
    
    default: throw "Unsupported expression <e>";
  }
}