/* global CodeMirror */
GLOBAL.CodeMirror = require("../node_modules/codemirror/addon/runmode/runmode.node.js");
require("../build/abap.js");

var chai = require("chai");
var expect = chai.expect;

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

describe("simple", function () {
    it("comment", function () {
        expect(syntax("* comment")).to.equals("comment");
    });
});