/// <reference path="../typings/main.d.ts"/>

const COMMENT  = "comment";
const STRING   = "string";
const NUMBER   = "number";
const KEYWORD  = "keyword";
const OPERATOR = "operator";
const ERROR    = "error";

class State {
    public mode: boolean;
}

class AbapMode implements CodeMirror.Mode<State> {

    public startState: () => State;

    public constructor() {
        this.startState = () => { return new State(); };
    }

    public token(stream: CodeMirror.StringStream, state: State) {

        if (stream.eatSpace()) { return undefined; };

        if (this.isKeyword(stream)) {
            return KEYWORD;
        } else if (stream.match(/^\d+( |\.|$)/, false)) {
            stream.match(/^\d+/);
            return NUMBER;
        } else if (stream.match(/^##\w+/)) {
// pragmas
            return COMMENT;
        }

        let ch = stream.next();
        let peek = stream.peek();
        if (peek === undefined) {
            peek = "";
        }
        let col = stream.column();

        if ((ch === "*" && col === 0) || ch === "\"") {
            stream.skipToEnd();
            return COMMENT;
        } else if (this.isOperator(ch + peek)) {
            if (peek !== " ") {
                stream.next();
            }
            return OPERATOR;
        } else if (ch === "\'") {
            let next = "";
            while (next !== undefined) {
                if (next === "\'") {
                    state.mode = false;
                    break;
                }
                next = stream.next();
            }
            return STRING;
        } else if (ch === "|") {
            let next = "";
            while (next !== undefined) {
                if (next === "|") {
                    state.mode = false;
                    break;
                }
                next = stream.next();
            }
            return STRING;
        } else {
            stream.eatWhile(/(\w|<|>)/);
            return ERROR;
        }
    };

    private isOperator(str: string): boolean {
        const OPERATORS = "?= = > <> < + - * / &&";

        str = str.trim();

        let list = OPERATORS.split(" ");

        for (let i = 0; i < list.length; i++) {
            if (str === list[i]) {
                return true;
            }
        }
        return false;
    }

    private isKeyword(stream: CodeMirror.StringStream): boolean {

        const KEYWORDS =
            "IS NOT EQ GE GT REF " +
            "AND ALIAS ALIASES APPEND ASCENDING ASSERT ASSIGN ASSIGNING " +
            "BACK BEGIN BINARY BLOCK BOUND BY BYTE " +
            "CALL CHANGING CHECK CLEAR CLOSE CNT COLLECT COMMIT CHARACTER " +
            "CORRESPONDING COMMUNICATION COMPONENT COMPUTE CONCATENATE CONDENSE CONSTANTS " +
            "CONTROLS CONVERT CREATE CURRENCY " +
            "DATA DESCENDING DEFAULT DEFINE DEFINITION DEFERRED DELETE DESCRIBE DETAIL DIVIDE DURATION " +
            "DELETING " +
            "END ENDEXEC ENDFUNCTION " +
            "ENDCLASS ENDMETHOD ENDFORM " +
            "CLASS METHOD FORM " +
            "ENDINTERFACE ENDMODULE " +
            "ENDPROVIDE ENDSELECT ENDTRY ENDWHILE EVENT EVENTS EXEC EXIT EXPORT " +
            "EXPORTING EXTRACT EXCEPTION EXCEPTIONS " +
            "FRAME FETCH FIELDS FINAL FORMAT FREE FROM FUNCTION FIND FOR " +
            "GENERATE " +
            "HARMLESS HIDE " +
            "IMPORT IMPORTING INDEX INFOTYPES INITIAL INITIALIZATION " +
            "INTERFACE INTERFACES INPUT INSERT IMPLEMENTATION INTO " +
            "LEAVE LEVEL LIKE LINE LOAD LOCAL LENGTH LEFT LEADING " +
            "METHOD MESSAGE METHODS MODIFY MODULE MOVE MULTIPLY MATCH " +
            "NEW " +
            "OBJECT OBLIGATORY OVERLAY OPTIONAL OTHERS OCCURRENCES OCCURS OFFSET " +
            "PACK PARAMETERS PERFORM POSITION PRIVATE PROGRAM PROTECTED PROVIDE PUBLIC " +
            "RADIOBUTTON RAISING RANGES RECEIVE RECEIVING REDEFINITION REF " +
            "REFERENCE REFRESH REGEX REJECT RESULTS " +
            "REPLACE REPORT RESERVE RESTORE RETURN RETURNING RISK ROLLBACK READ " +
            "SCAN SCROLL SEARCH SELECT SEPARATED SHIFT SHORT SINGLE SKIP SORT SORTED SPLIT STANDARD " +
            "STATICS STEP STOP STRUCTURE SUBMATCHES SUBMIT SUBTRACT SUMMARY SUPPRESS SECTION " +
            "TABLES TABLE TESTING TIMES TITLE TITLEBAR TO TRANSFER TRANSFORMATION TRANSLATE TYPES TYPE " +
            "UNASSIGN ULINE UNPACK UPDATE USING " +
            "VALUE " +
            "WHEN WHILE WINDOW WRITE WHERE WITH " +
            "ADD-CORRESPONDING AUTHORITY-CHECK " +
            "BREAK-POINT CLASS-DATA " +
            "CLASS-METHOD CLASS-METHODS " +
            "DIVIDE-CORRESPONDING DISPLAY DISPLAY-MODE " +
            "EDITOR-CALL END-OF-DEFINITION END-OF-PAGE END-OF-SELECTION " +
            "FIELD-GROUPS FIELD-SYMBOLS " +
            "FUNCTION-POOL LEFT-JUSTIFIED LINE-COUNT LINE-SIZE " +
            "MESSAGE-ID MOVE-CORRESPONDING MULTIPLY-CORRESPONDING " +
            "NEW-LINE NEW-PAGE NEW-SECTION " +
            "NO-GAP NO-SIGN " +
            "NO-ZERO PRINT-CONTROL " +
            "READ-ONLY RIGHT-JUSTIFIED " +
            "SELECT-OPTIONS SELECTION-SCREEN START-OF-SELECTION " +
            "SUBTRACT-CORRESPONDING SYNTAX-CHECK " +
            "SYNTAX-TRACE SYSTEM-CALL TOP-OF-PAGE TYPE-POOL TYPE-POOLS " +
            "AT CASE CATCH CONTINUE DO ELSEIF ELSE ENDAT ENDCASE ENDDO ENDIF " +
            "ENDLOOP ENDON IF LOOP ON RAISE TRY";

        let list = KEYWORDS.split(" ");

        let match = false;
        for (let i = 0; i < list.length; i++) {
            let reg = new RegExp("^" + list[i] + "( |\\\.|,|:|$)", "i");
            if (stream.match(reg, false)) {
                let reg = new RegExp("^" + list[i], "i");
                stream.match(reg);
                match = true;
                break;
            }
        }
        return match;
    }
}

function ABAPFactory(options: CodeMirror.EditorConfiguration, spec: State): CodeMirror.Mode<State> {
    return new AbapMode();
}

CodeMirror.defineMode("abap", ABAPFactory);