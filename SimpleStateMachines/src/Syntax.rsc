module Syntax

extend lang::std::Layout;
extend lang::std::Id;

start syntax Machine
  = "machine" Id State*;
  
syntax State
  = "state" Id Transition*;
  
syntax Transition
  = "on" Id "goto" Id;

