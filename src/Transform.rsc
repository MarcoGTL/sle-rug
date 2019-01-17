module Transform

extend lang::std::Id;
import Resolve;
import AST;
import IO;
import Set;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (a && b) q1: "" int;
 *     if (a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) = form(f.name, [*flatten(q, boolean(true)) | AQuestion q <- f.questions]);

list[AQuestion] flatten(AQuestion q, AExpr cond) {
  switch (q) {
    case single(str label, str name, AType datatype): return [ifthen(cond, [q])];
    case computed(str label, str name, AType datatype, AExpr expr): return [ifthen(cond, [q])];
    case block(list[AQuestion] questions): return [flatten(qu, cond)|AQuestion qu <- q.questions];
    case ifthen(AExpr condition, list[AQuestion] questions): 
      return [*flatten(qu, and(cond, q.condition))|AQuestion qu <- questions];
    case ifthenelse(AExpr condition, list[AQuestion] ifquestions, list[AQuestion] elsequestions):
      return [*flatten(qu, and(cond, q.condition))|AQuestion qu <- ifquestions]
            +[*flatten(qu, not(and(cond, q.condition)))|AQuestion qu <- elsequestions];
  }
  return q;
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
 start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
   set[loc] occurrences = {l1 | <loc l1, loc l2> <- useDef, l2 == useOrDef} 
                        + {l2 | <loc l1, loc l2> <- useDef, l1 == useOrDef} 
                        + useOrDef;
   return visit(f) {
     case Id x => [Id]newName when (x@\loc) in occurrences
   }
 }