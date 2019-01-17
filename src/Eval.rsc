module Eval

import AST;
import Resolve;
import IO;

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
 VEnv venv = ( q.name : vint(0)  | /AQuestion q <- f.questions, (q has datatype), (q.datatype ==  atint()));
 venv += ( q.name : vbool(false)  | /AQuestion q <- f.questions, (q has datatype), (q.datatype ==  atbool()));
 venv += ( q.name : vstr("")  | /AQuestion q <- f.questions, (q has datatype), (q.datatype ==  atstr()));
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
  for (/AQuestion q <- f.questions) {
    venv += eval(q, inp, venv);
  }
  return venv;
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching 
  if (q is ifthen) {
    if (eval(q.condition, venv) == vbool(true)) {
      for (AQuestion qe <- q.questions) {
        venv += eval(qe, inp, venv);
      }
    }
  }
  if (q is ifthenelse) {
    if (eval(q.condition, venv) == vbool(true)) {
      for (AQuestion qe <- q.elsequestions + q.ifquestions) {
        venv += eval(qe, inp, venv);
      }
    }
  }
  // evaluate input and computed questions to return updated VEnv
  if (q is single && inp.question == q.name) {
    venv[inp.question] = inp.\value;
  }
  if (q is computed) {
    venv[q.name] = eval(q.expr, venv);
  }
  return venv; 
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case parentheses(AExpr expr): return eval(expr, venv);
    case ref(str x): return venv[x];
    case integer(int x): return vint(x);
    case boolean(bool boolean): return vbool(boolean);
    case string(str string): return vstr(string);
    case not(AExpr expr): 
      switch(eval(expr, venv)) {
    	case vbool(x): return vbool(!x);
    	default: throw "Cannot negate <expr.name>: <eval(expr, venv)>";
      }
    case product(AExpr expr1, AExpr expr2):
	  switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vint(x * y);
	    default: throw "Cannot multiply <expr1.name>: <eval(expr1, venv)> by <expr2.name>: <eval(expr2,venv)>";
	  }
    case quotient(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vint(x / y);
	    default: throw "Cannot divide <expr1.name>: <eval(expr1, venv)> by <expr2.name>: <eval(expr2,venv)>";
	  }
    case plus(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vint(x + y);
	    default: throw "Cannot add <expr1.name>: <eval(expr1, venv)> to <expr2.name>: <eval(expr2,venv)>";
	  }
    case minus(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vint(x - y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    case less(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x < y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    case lesseq(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x <= y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    case greater(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x > y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    case greatereq(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x >= y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    case equals(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x == y);
	    case [vbool(x), vbool(y)]: return vbool(x == y);
	    case [vstr(x), vstr(y)]: return vbool(x == y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    case notequals(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vint(x), vint(y)]: return vbool(x != y);
	    case [vbool(x), vbool(y)]: return vbool(x != y);
	    case [vstr(x), vstr(y)]: return vbool(x != y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    case and(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vbool(x), vbool(y)]: return vbool(x && y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    case or(AExpr expr1, AExpr expr2):
      switch([eval(expr1, venv), eval(expr2, venv)]) {
	    case [vbool(x), vbool(y)]: return vbool(x || y);
	    default: throw "Cannot compare <expr1.name>: <eval(expr1, venv)> with <expr2.name>: <eval(expr2,venv)>";
	  }
    
    default: throw "Unsupported expression <e>";
  }
}