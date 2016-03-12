/// <reference path="../typings/codemirror/codemirror.d.ts" />

const COMMENT  = "comment";
const STRING   = "string";
// const NUMBER   = "number";
const KEYWORD  = "keyword";
// const OPERATOR = "operator";
const ERROR    = "error";

class AbapMode implements CodeMirror.Mode<any> {

    public token(stream: CodeMirror.StringStream, state: any) {

        if (stream.eatSpace()) { return undefined; };

        if (this.isKeyword(stream)) {
            return KEYWORD;
        }

        let ch = stream.next();
        let col = stream.column();

        if ((ch === "*" && col === 0) || ch === "\"") {
            stream.skipToEnd();
            return COMMENT;
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
        }

        return ERROR;
    };

    private isKeyword(stream: CodeMirror.StringStream) {

        const KEYWORDS =
            "ALIAS ALIASES APPEND ASCENDING ASSERT ASSIGN ASSIGNING " +
            "BACK BEGIN BINARY BOUND BYTE " +
            "CALL CHANGING CHECK CLEAR CLOSE CNT COLLECT COMMIT CHARACTER " +
            "CORRESPONDING COMMUNICATION COMPONENT COMPUTE CONCATENATE CONDENSE CONSTANTS " +
            "CONTROLS CONVERT CREATE CURRENCY " +
            "DATA DESCENDING DEFAULT DEFINE DEFINITION DEFERRED DELETE DESCRIBE DETAIL DIVIDE " +
            "DELETING " +
            "END ENDEXEC ENDFUNCTION " +
            "ENDCLASS ENDMETHOD ENDFORM " +
            "CLASS METHOD FORM " +
            "ENDINTERFACE ENDMODULE " +
            "ENDPROVIDE ENDSELECT ENDTRY ENDWHILE EVENT EVENTS EXEC EXIT EXPORT " +
            "EXPORTING EXTRACT EXCEPTION EXCEPTIONS " +
            "FETCH FIELDS FORMAT FREE FROM FUNCTION FIND " +
            "GENERATE " +
            "HIDE " +
            "IMPORT IMPORTING INDEX INFOTYPES INITIAL INITIALIZATION " +
            "INTERFACE INTERFACES INPUT INSERT IMPLEMENTATION INTO " +
            "LEAVE LIKE LINE LOAD LOCAL LENGTH LEFT LEADING " +
            "METHOD MESSAGE METHODS MODIFY MODULE MOVE MULTIPLY MATCH " +
            "OBJECT OBLIGATORY OVERLAY OPTIONAL OTHERS OCCURRENCES OCCURS OFFSET " +
            "PACK PARAMETERS PERFORM POSITION PRIVATE PROGRAM PROTECTED PROVIDE PUBLIC " +
            "RADIOBUTTON RAISING RANGES RECEIVE RECEIVING REDEFINITION REFERENCE REFRESH REGEX REJECT RESULTS " +
            "REPLACE REPORT RESERVE RESTORE RETURN RETURNING ROLLBACK READ " +
            "SCAN SCROLL SEARCH SELECT SEPARATED SHIFT SINGLE SKIP SORT SORTED SPLIT STANDARD " +
            "STATICS STEP STOP STRUCTURE SUBMATCHES SUBMIT SUBTRACT SUMMARY SUPPRESS SECTION " +
            "TABLES TABLE TIMES TITLEBAR TRANSFER TRANSFORMATION TRANSLATE TYPES TYPE " +
            "UNASSIGN ULINE UNPACK UPDATE USING " +
            "VALUE " +
            "WHEN WHILE WINDOW WRITE WHERE WITH " +
            "ADD-CORRESPONDING AUTHORITY-CHECK " +
            "BREAK-POINT CLASS-DATA " +
            "CLASS-METHOD CLASS-METHODS " +
            "DIVIDE-CORRESPONDING DISPLAY-MODE " +
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
            let reg = new RegExp("^" + list[i] + "( |\\\.|:)", "i");
            if (stream.match(reg) !== null) {
                match = true;
                break;
            }
        }
        return match;
    }
}

function factory(options: CodeMirror.EditorConfiguration, spec: any): CodeMirror.Mode<any> {
    return new AbapMode();
}

CodeMirror.defineMode("abap", factory);