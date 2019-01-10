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
  = single(str label, str name, AType datatype)
  | computed(str label, str name, AType datatype, AExpr expr)
  | block(list[AQuestion] questions)
  | ifthen(AExpr condition, list[AQuestion] questions)
  |	ifthenelse(AExpr condition, list[AQuestion] ifquestions, list[AQuestion] elsequestions)
  ; 

data AExpr(loc src = |tmp:///|)
  = parentheses(AExpr expr)
  | ref(str name)
  | integer(int integer)
  | boolean(bool boolean)
  | string(str string)
  | not(AExpr expr)
  | product(AExpr expr1, AExpr expr2)
  | quotient(AExpr expr1, AExpr expr2)
  | plus(AExpr expr1, AExpr expr2)
  | minus(AExpr expr1, AExpr expr2)
  | less(AExpr expr1, AExpr expr2)
  | lesseq(AExpr expr1, AExpr expr2)
  | greater(AExpr expr1, AExpr expr2)
  | greatereq(AExpr expr1, AExpr expr2)
  | equals(AExpr expr1, AExpr expr2)
  | notequals(AExpr expr1, AExpr expr2)
  | and(AExpr expr1, AExpr expr2)
  | or(AExpr expr1, AExpr expr2)
  ;

data AType(loc src = |tmp:///|)
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;