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
    it("keyword", () => { expect(syntax("WRITE")).to.equals("keyword"); });
    it("string", () => { expect(syntax("'hello world'")).to.equals("string"); });
    it("keyword", () => { expect(syntax("endclass")).to.equals("keyword"); });
    it("number1", () => { expect(syntax("123")).to.equals("number"); });
    it("number2", () => { expect(syntax("1")).to.equals("number"); });
    it("operator", () => { expect(syntax("=")).to.equals("operator"); });
});

describe("multi", () => {
    it("if", () => { expect(syntax("IF 5 > 2")).to.equals("keyword number operator number"); });
    it("write", () => { expect(syntax("write 'hello world'")).to.equals("keyword string"); });
});
