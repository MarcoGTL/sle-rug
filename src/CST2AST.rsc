module CST2AST

import Syntax;
import AST;
import IO;

import ParseTree;
import String;
import Boolean;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

// remove layout before and after form
AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  switch (f) {
    case (Form)`form <Id x> { <Question* qs> }`: return form("", [], src=f@\loc);
    default: throw "unhandled expression: <f>";
  }
}

AForm cst2ast(f:(Form)`form <Id x> { <Question* qs> }`)
  = form("<x>", [cst2ast(q) | Question q <- qs], src = f@\loc);

AQuestion cst2ast(qe:Question q) {
  switch (q) {
  	case (Question)`<Str label> <Id variable>:<Type t>`: return single("<label>"[1..-1], "<variable>", cst2ast(t), src=qe@\loc);
	case (Question)`<Str label> <Id variable>: <Type t> = <Expr e>`: return computed("<label>"[1..-1], "<variable>", cst2ast(t), cst2ast(e), src=qe@\loc);
	case (Question)`{<Question* qs>}`: return block([cst2ast(qu) | Question qu <- qs], src=qe@\loc); 
	case (Question)`if (<Expr e>) {<Question* qs>}`: return ifthen(cst2ast(e), [cst2ast(qu) | Question qu <- qs], src=qe@\loc);
	case (Question)`if (<Expr e>) {<Question* qs1>} else {<Question* qs2>}`: return ifthenelse(cst2ast(e), [cst2ast(q1) | Question q1 <- qs1], [cst2ast(q2) | Question q2 <- qs2], src=qe@\loc);
	default: throw "unhandled expression: <q>";
  }
}

AExpr cst2ast(ex:Expr e) {
  switch (e) {
  	case (Expr)`(<Expr expr>)`: return parentheses(cst2ast(expr), src=ex@\loc);
    case (Expr)`<Id x>`: return ref("<x>", src=ex@\loc);
    case (Expr)`<Int x>`: return integer(toInt("<x>"), src=ex@\loc);
    case (Expr)`<Bool x>`: return boolean(fromString("<x>"), src=ex@\loc);
    case (Expr)`<Str x>`: return string("<x>"[1..-1], src=ex@\loc);
    case (Expr)`!<Expr expr>`: return not(cst2ast(expr), src=ex@\loc);
    case (Expr)`<Expr e1> * <Expr e2>`: return product(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> / <Expr e2>`: return quotient(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> + <Expr e2>`: return plus(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> - <Expr e2>`: return minus(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> \< <Expr e2>`: return less(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> \<= <Expr e2>`: return lesseq(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> \> <Expr e2>`: return greater(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> \>= <Expr e2>`: return greatereq(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> == <Expr e2>`: return equals(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> != <Expr e2>`: return notequals(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> && <Expr e2>`: return and(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    case (Expr)`<Expr e1> || <Expr e2>`: return or(cst2ast(e1), cst2ast(e2), src=ex@\loc);
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(ty:Type t) {
  switch(t) {
  	case (Type)`integer`: return tint();
  	case (Type)`boolean`: return tbool();
  	case (Type)`string`: return tstr();
  	default: return tunknown();
  }
}
