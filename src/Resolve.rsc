module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

// the reference graph
alias UseDef = rel[loc use, loc def];

UseDef resolve(AForm f) = uses(f) o defs(f);

Use uses(AForm f) {
  Use use = {};  
  for (/AExpr e := f) {
  	if (e has name) {
  	  use[e.src] = e.name;
  	}
  }  
  return use; 
}

Def defs(AForm f) {
  Def def = {};  
  for (/AQuestion q := f) {
  	if (q has name) {
  	  def[q.name] = q.src;
  	}
  } 
  return def;
}