module Plugin

import util::IDE;
import ParseTree;
import Syntax;
import AST;
import CST2AST;
import Resolve;
import Check;

anno rel[loc, loc] Tree@hyperlinks;

void main() {
  registerLanguage("State Machines", "statemachine", start[Machine](str src, loc l) {
    return parse(#start[Machine], src, l);
  });
  
  contribs = {
    annotator(Tree(Tree t) {
      if (start[Machine] pt := t) {
        AMachine ast = cst2ast(pt);
        UseDef useDef = resolve(ast);
        RefGraph r = resolve2(ast);
        msgs = check(ast, r);
        return t[@hyperlinks=useDef][@messages=msgs];
      }
      return t[@messages={error("Not a machine", t@\loc)}];
    })
  };
  
  registerContributions("State Machines", contribs);
}