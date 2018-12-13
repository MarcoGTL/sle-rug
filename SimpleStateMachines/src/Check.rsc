module Check

import AST;
import Message;
import Set;

// p.m.
alias UseDef = rel[loc use, loc def];

alias RefGraph = tuple[
  rel[loc, str] uses, 
  rel[str, loc] defs, 
  rel[loc, loc] useDef
]; 

RefGraph resolve2(AMachine m) {
  // collect definitions in an environment (NB: rel, not map!)
  rel[str, loc] defs = { <s.name, s.src> | AState s <- m.states };

  // the transitions that act as use occurrences
  rel[loc, str] uses = { <t.src, t.target> |  /ATransition t := m };

  // return the reference graph
  return <uses, defs, uses o defs>; 
}

set[Message] check(AMachine m, RefGraph r) 
  = ( {} | it + check(s, r) | AState s <- m.states );

set[Message] check(AState s, RefGraph r) 
  = { error("Duplicate state", s.src) | size(r.defs[s.name]) > 1 }
  + { warning("Unused state", s.src) | s.src notin r.useDef<1> }
  + ( {} | it + check(t, r) | ATransition t <- s.transitions );


set[Message] check(ATransition t, RefGraph r) 
  = { error("Undefined target state", t.src) | t.target notin r.defs<0> };