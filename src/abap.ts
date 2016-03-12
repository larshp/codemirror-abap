/// <reference path="../typings/codemirror/codemirror.d.ts" />

import * as CodeMirror from 'codemirror';

const COMMENT = "comment";
const STRING = "string";
const NUMBER = "number";
const KEYWORD = "keyword";
const OPERATOR = "operator";
const TEXT = "text";

class AbapMode implements CodeMirror.Mode<any> {

    public token(stream: CodeMirror.StringStream, state: any) {

        while (stream.eatSpace()) {}

        if (stream.match(/^\*/)) {
            stream.skipToEnd();
            return COMMENT;
        }

        stream.skipToEnd();

        return TEXT;
    };

}

function factory(options: CodeMirror.EditorConfiguration, spec: any): CodeMirror.Mode<any> {
    return new AbapMode();
}

CodeMirror.defineMode("abap", factory);