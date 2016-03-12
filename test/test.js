/* global CodeMirror */
GLOBAL.CodeMirror = require("../node_modules/codemirror/addon/runmode/runmode.node.js");
require("../build/abap.js");

function syntax(code) {
    var res = "";
    CodeMirror.runMode(code, "abap", function(text, style) {
        if(res.length === 0) {
            res = style;
        } else {
            res = res + " " + style;
        }
    });
    return res;
}

console.log(syntax("* comment"));