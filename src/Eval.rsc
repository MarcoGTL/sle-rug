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
  
Value toValue(AType t){
 switch(t) {
    case tint(): return vint();
    case tbool(): return vbool();
  	case tstr(): return vstr();
  	default: return;
  }
 }

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
    
    // etc.
    
    default: throw "Unsupported expression <e>";
  }
}