module Compile

import AST;
import Resolve;
import Set;
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
    default: inp = input();
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

  code += "\n    },
  		  '    methods: {";
//Insert functions for the computed questions	  
  code += "\n    }
          '})";;
  return code;
}
              
str template() {
  str thing = "var vm = new Vue({
              '    el: \'#vm\',
              '    data: {
              '    },
              '";
     thing += "    methods: {
              '    }
              '})";
  return thing;
}