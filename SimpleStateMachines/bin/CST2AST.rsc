module CST2AST

import Syntax;
import AST;

// for @\loc annotation
import ParseTree; 

// strip off layout before and after the actual Machine
AMachine cst2ast(start[Machine] m) = cst2ast(m.top);

AMachine cst2ast(m:(Machine)`machine <Id x> <State* ss>`)
  = machine("<x>", [ cst2ast(s) | State s <- ss ]
      , src = m@\loc);

AState cst2ast(s:(State)`state <Id x> <Transition* ts>`)
  = state("<x>", [ cst2ast(t) | Transition t <- ts ]
      , src = s@\loc);

ATransition cst2ast(tr:(Transition)`on <Id e> goto <Id t>`)
  = transition("<e>", "<t>", src = tr@\loc);



