module AST

data AMachine(loc src = |file:///|)
  = machine(str name, list[AState] states);

data AState(loc src = |file:///|)
  = state(str name, list[ATransition] transitions);

data ATransition(loc src = |file:///|)
  = transition(str event, str target);

AMachine doors() 
  = machine("Doors", [
      state("closed", [transition("open", "opened")]),
      state("opened", [transition("close", "closed")])
    ]);

