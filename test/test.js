/* global CodeMirror */
GLOBAL.CodeMirror = require("../node_modules/codemirror/addon/runmode/runmode.node.js");
require("../build/abap.js");

var chai = require("chai");
var expect = chai.expect;

function syntax(code) {
    var res = "";
    CodeMirror.runMode(code, "abap", function(text, style) {
        if(style === undefined) {
            return; // skip space
        } else if(res.length === 0) {
            res = style;
        } else {
            res = res + " " + style;
        }
    });
    return res;
}

describe("simple", () => {
    it("comment", () => { expect(syntax("* comment")).to.equals("comment"); });
    it("comment", () => { expect(syntax("\" comment")).to.equals("comment"); });
    it("comment", () => { expect(syntax(" \" comment")).to.equals("comment"); });
    it("keyword", () => { expect(syntax("WRITE ")).to.equals("keyword"); });
    it("string", () => { expect(syntax("'hello world'")).to.equals("string"); });
});

describe("multi", () => {
    it("write", () => { expect(syntax("write 'hello world'")).to.equals("keyword string"); });
});