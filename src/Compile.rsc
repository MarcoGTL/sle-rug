module Compile

import AST;
import Resolve;
import Set;
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

HTML5Node question(AQuestion q) {
  HTML5Node inp;
  switch(q.datatype) {
    case tbool(): inp = input(\type("checkbox"), name(q.name));
    case tint(): inp = input(\type("number"), name(q.name), placeholder(0));
    case tstr(): inp = input(\type("text"), name(q.name), placeholder(q.name));
    default: throw "Unsupported datatype <q.datatype>";
  }
  HTML5Node question = div(hr(), label(q.label), br(), q.name, ": ", inp);
  return question;
}

HTML5Node form2html(AForm f) {
  HTML5Node html 
    = html(head(title(f.name)), body(h1(f.name), 
    	form([question(q)|AQuestion q <- f.questions, q has name]), 
    		input(\type("submit"), \value("Submit"))));
  return html;
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
	  case tint():  code += "0";
	  case tbool(): code += "false";
	  case tstr():  code += "\'\'";
	  default: throw "Unsupported datatype <q.datatype>";
	}
  }
  code += "\n    },
  		  '    methods: {";
//Insert functions for the computed questions
  for (/AQuestion q <- f.questions, q is computed) {
	code += "\n        "+ q.name + ": function() {
			'            return " + exp2js(q.expr) + ";
			'        }";
  }
  code += "\n    }
          '})";;
  return code;
}

str exp2js(AExpr expr) {
  println(expr);
  switch(expr) {
    case parentheses(AExpr e): return "(" + exp2js(e) + ")";
    case ref(str name): return "eval(this." + name + ")";
    case integer(int integer): return "<integer>";
    case boolean(bool boolean): return toString(boolean);
    case string(str string): return string;
    case not(AExpr e): return "!(" + exp2js(e) + ")";
    case product(AExpr e1, AExpr e2): return exp2js(e1) + " * " + exp2js(e2);
    case quotient(AExpr e1, AExpr e2): return exp2js(e1) + " / " + exp2js(e2);
    case plus(AExpr e1, AExpr e2): return exp2js(e1) + " + " + exp2js(e2);
    case minus(AExpr e1, AExpr e2): return exp2js(e1) + " - " + exp2js(e2);
    case less(AExpr e1, AExpr e2): return exp2js(e1) + " \< " + exp2js(e2);
    case lesseq(AExpr e1, AExpr e2): return exp2js(e1) + " \<= " + exp2js(e2);
    case greater(AExpr e1, AExpr e2): return exp2js(e1) + " \> " + exp2js(e2);
    case greatereq(AExpr e1, AExpr e2): return exp2js(e1) + " \>= " + exp2js(e2);
    case equals(AExpr e1, AExpr e2): return exp2js(e1) + " === " + exp2js(e2);
    case notequals(AExpr e1, AExpr e2): return exp2js(e1) + " != " + exp2js(e2);
    case and(AExpr e1, AExpr e2): return exp2js(e1) + " && " + exp2js(e2);
    case or(AExpr e1, AExpr e2): return exp2js(e1) + " || " + exp2js(e2);
    default: throw "Unknown expression encountered: <expr>";
  }
  return "";
}