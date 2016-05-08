
/*(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../node_modules/codemirror/lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../node_modules/codemirror/lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
*/
    CodeMirror.defineMode("abap", ABAPFactory);
//});