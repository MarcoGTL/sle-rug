module Resolve

import AST;

alias UseDef = rel[loc use, loc def];


UseDef resolve(AMachine m) {
  rel[str, loc] def = { <s.name, s.src> | AState s <- m.states };
  
  UseDef useDef = {};
  
  for (/ATransition t := m) {
    useDef += { <t.src, s> | loc s <- def[t.target] };
  }

  return useDef;
} 