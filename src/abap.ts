/// <reference path="../typings/codemirror/codemirror.d.ts" />

import * as CodeMirror from 'codemirror';

class AbapMode implements CodeMirror.Mode<any> {

    public token(stream: CodeMirror.StringStream, state: any) {
        return "text";
    };

}

function factory(options: CodeMirror.EditorConfiguration, spec: any): CodeMirror.Mode<any> {
    return new AbapMode();
}

CodeMirror.defineMode("abap", factory);