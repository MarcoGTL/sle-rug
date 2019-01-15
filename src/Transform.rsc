module Transform

import Resolve;
import AST;
import IO;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; if (a) { if (b) { q1: "" int; } q2: "" int; }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (a && b) q1: "" int;
 *     if (a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) = form(f.name, [flatten(q, boolean(true)) | AQuestion q <- f.questions]);

AQuestion flatten(AQuestion q, AExpr cond) {
  switch (q) {
    case single(str label, str name, AType datatype): return ifthen(cond, [q]);
    case computed(str label, str name, AType datatype, AExpr expr): return ifthen(cond, [q]);
    case block(list[AQuestion] questions): return [flatten(qu, cond)|AQuestion qu <- q.questions];
    case ifthen(AExpr condition, list[AQuestion] questions): 
      if (cond == boolean(true)) {
        return block([flatten(qu, q.condition)|AQuestion qu <- questions]);
      } else {
        return block([flatten(qu, and(cond, q.condition))|AQuestion qu <- questions]);
      }
    case ifthenelse(AExpr condition, list[AQuestion] ifquestions, list[AQuestion] elsequestions):
      if (cond == boolean(true)) {
      	return block([flatten(qu, q.condition)|AQuestion qu <- ifquestions]
      				+[flatten(qu, not(q.condition))|AQuestion qu <- elsequestions]);
      } else {
        return block([flatten(qu, and(cond, q.condition))|AQuestion qu <- ifquestions]
                    +[flatten(qu, not(and(cond, q.condition)))|AQuestion qu <- elsequestions]);
      }
    default: throw "unknown question: <q>";
  }
  return q;
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 * Bonus: do it on concrete syntax trees.
 */
 
 AForm rename(AForm f, loc useOrDef, str newName, UseDef useDef) {
   return f; 
 } 
 
 
 

