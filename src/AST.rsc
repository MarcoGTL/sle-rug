module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = single(str label, str variable, AType)
  | computed(str label, str variable, AType, AExpr)
  | block(list[AQuestion] questions)
  | ifthen(AExpr, list[AQuestion] questions)
  |	ifthenelse(AExpr, list[AQuestion] questions, list[AQuestion] questions)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(str name)
  | integer(int variable)
  | boolean(bool boolean)
  | not(AExpr)
  | product(AExpr, AExpr)
  | quotient(AExpr, AExpr)
  | plus(AExpr, AExpr)
  | minus(AExpr, AExpr)
  | less(AExpr, AExpr)
  | lesseq(AExpr, AExpr)
  | greater(AExpr, AExpr)
  | greatereq(AExpr, AExpr)
  | equals(AExpr, AExpr)
  | notequals(AExpr, AExpr)
  | and(AExpr, AExpr)
  | or(AExpr, AExpr)
  ;

data AType(loc src = |tmp:///|)
 = datatype(str name);

AForm form()
 = form("test", [
 	 single("Did you buy a house in 2010?", "hasBoughtHouse", "boolean"),
 	 computed("Did you enter a loan?", "hasMaintLoan", "boolean", not(boolean(true)))
   ]);