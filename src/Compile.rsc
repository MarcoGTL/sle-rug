module Compile

import AST;
import Resolve;
import String;
import Boolean;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

void printtest(HTML5Node tnode) {
  println(toString(tnode));
}

HTML5Attr vmodel(value val) = html5attr("v-model", val);
HTML5Attr vif(value val) = html5attr("v-if", val);

HTML5Node form2html(AForm f) = 
	html(
      script(src("https://cdn.jsdelivr.net/npm/vue")),
	  head(title(f.name)),
	  body(
	    h3(f.name),
	    div(
	      id("vm"),
	      form(
	        div([question(f, q, "true")|AQuestion q <- f.questions]),
	        input(
	          \type("submit"), 
	          \value("Submit")
	        )
	      )
	    ),
	    script(src(f.src.file[..-4]+"js"))
	  )
	);
    
HTML5Attr cond(str condition) {
	if (condition == "") {
		return vif(true);
	} else {
		return vif(condition);
	}
}

HTML5Node question(AForm f, AQuestion q, str condition) {
  if (q is single) {
    switch(q.datatype) {
    	case tbool(): return  p(cond(condition), HTML5Node::label(q.label), input(\type("checkbox"), id(q.name), vmodel(q.name)));
    	case tint(): return  p(cond(condition), HTML5Node::label(q.label), input(\type("number"), id(q.name), vmodel(q.name)));
    	case tstr(): return  p(cond(condition), HTML5Node::label(q.label), input(\type("text"), id(q.name), vmodel(q.name)));
    	default: throw "undefined datatype <q.datatype>";
    }
  }
  if (q is computed ) {
  	return p(cond(condition), HTML5Node::label(q.label), "{{<q.name>()}}");
  }
  if (q is ifthen) {
  	return div([question(f, qe, condition + " && " + exp2js(f, q.condition, true))|AQuestion qe <- q.questions]);
  }
  if (q is ifthenelse) {
  	return div([question(f, qe, condition + " && " + exp2js(f, q.condition, true))|AQuestion qe <- q.ifquestions]
  	          +[question(f, qe, condition + " && " + "!(" + exp2js(f, q.condition, true) + ")")|AQuestion qe <- q.elsequestions]);
  }
  return p();
}

str form2js(AForm f) {
  set[str] variables = defs(f)<0>;
  str code = "var vm = new Vue({
              '    el: \'#vm\',
              '    data: {";
//Insert non-computed question variables
  for (/AQuestion q <- f.questions, q is single) {
	code += "\n        "+ q.name + ": ";
	switch(q.datatype) {
	  case tint():  code += "0,";
	  case tbool(): code += "false,";
	  case tstr():  code += "\'\',";
	  default: throw "Unsupported datatype <q.datatype>";
	}
  }
  code += "\n    },
  		  '    methods: {";
//Insert functions for the computed questions
  for (/AQuestion q <- f.questions, q is computed) {
	code += "\n        "+ q.name + ": function() {
			'            return " + exp2js(f, q.expr, false) + ";
			'        },";
  }
  code += "\n    }
          '})";
  return code;
}

bool isComputed(str variable, AForm f) {
  for (/AQuestion q <- f.questions, q is computed && q.name == variable) {
    return true;
  }
  return false;
}

str exp2js(AForm f, AExpr expr, bool condition) {
  switch(expr) {
    case parentheses(AExpr e): return "(" + exp2js(f, e, condition) + ")";
    case ref(str name): 
      if (condition) {
        return isComputed(name, f) ? name+"()" : name;
        } else {
        return  isComputed(name, f) ? "eval(this." + name + "())" : "eval(this." + name + ")";
      }
    case integer(int integer): return "<integer>";
    case boolean(bool boolean): return toString(boolean);
    case string(str name): return "\'" + name + "\'";
    case not(AExpr e): return "!(" + exp2js(f, e, condition) + ")";
    case product(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " * " + exp2js(f, e2, condition);
    case quotient(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " / " + exp2js(f, e2, condition);
    case plus(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " + " + exp2js(f, e2, condition);
    case minus(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " - " + exp2js(f, e2, condition);
    case less(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " \< " + exp2js(f, e2, condition);
    case lesseq(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " \<= " + exp2js(f, e2, condition);
    case greater(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " \> " + exp2js(f, e2, condition);
    case greatereq(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " \>= " + exp2js(f, e2, condition);
    case equals(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " === " + exp2js(f, e2, condition);
    case notequals(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " != " + exp2js(f, e2, condition);
    case and(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " && " + exp2js(f, e2, condition);
    case or(AExpr e1, AExpr e2): return exp2js(f, e1, condition) + " || " + exp2js(f, e2, condition);
    default: throw "Unknown expression encountered: <expr>";
  }
  return "";
}