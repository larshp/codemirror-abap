import CodeMirror from 'codemirror';
import 'codemirror/addon/runmode/runmode';
import { KEYWORDS, OPERATORS, COMMENT, STRING, NUMBER, KEYWORD, OPERATOR } from "./constants"


interface Keywords {
  [key: string]: boolean;
}
type AbapMode = CodeMirror.Mode<object>;

type CheckMatchCallback = (input: string) => boolean;

export const abapMode = (): AbapMode => {
  const composeKeywords = (words: string[]): Keywords =>
    words.reduce(
      (result, word) => ({
        ...result,
        [word]: true,
      }),
      {},
    );

  const keywords = composeKeywords(KEYWORDS);

  const checkMatch = (stream: CodeMirror.StringStream, separators: string | string[], callback: CheckMatchCallback): boolean => {
    let next = stream.next();
    let back = 0;
    while (true) {
      if (!next) {
        break;
      } else if (separators.includes(next)) {
        stream.backUp(1);
        break;
      } else {
        back++;
      }
      next = stream.next();
    }

    const toCheck = stream.current().toUpperCase();
    const match = callback(toCheck);
    if (match === false) {
      stream.backUp(back);
    }
    return match;
  }


  const isKeyword = (stream: CodeMirror.StringStream): boolean => {
    const checkKeyword: CheckMatchCallback = (input: string) =>
      keywords.propertyIsEnumerable(input);
    const KEYWORD_SEPARATORS = "(.,: ";

    return checkMatch(stream, KEYWORD_SEPARATORS, checkKeyword)
  };

  const isOperator = (stream: CodeMirror.StringStream): boolean => {
    const checkOperator: CheckMatchCallback
      = (input: string) => OPERATORS.includes(input);

    return checkMatch(stream, " ", checkOperator)
  };

  return {
    token: (stream: CodeMirror.StringStream, state: any) => {
      if (stream.eatSpace()) {
        return null;
      }

      if (isKeyword(stream)) {
        return KEYWORD;
      } else if (stream.match(/^\d+( |\.|$)/, false)) {
        stream.match(/^\d+/);
        return NUMBER;
      } else if (stream.match(/^##\w+/)) {
        // pragmas
        return COMMENT;
      }

      const char = stream.next();
      let peek = stream.peek();
      if (peek === undefined) {
        peek = '';
      }

      if ((char === '*' && stream.column() === 0) || char === '"') {
        stream.skipToEnd();
        return COMMENT;
      } else if (isOperator(stream)) {
        return OPERATOR;
      } else if (char === "'") {
        let next;
        next = '';
        while (next !== undefined) {
          if (next === "'") {
            state.mode = false;
            break;
          }
          next = stream.next();
        }
        return STRING;
      } else if (char === '|') {
        let next;
        next = '';
        while (next !== undefined) {
          if (next === '|') {
            state.mode = false;
            break;
          }
          next = stream.next();
        }
        return STRING;
      } else {
        stream.eatWhile(/(\w|<|>)/);
        return null;
      }
    },

    startState: () => ({
      mode: false,
    }),
  };
};

export const initAbapMode = (codemirror: any): void => {
  codemirror.defineMode('abap', abapMode);
  codemirror.defineMIME('text/abap', 'abap');
  const mimeType = {
    name: 'ABAP',
    mime: 'text/abap',
    mode: 'abap',
    ext: ['abap'],
  };

  codemirror.modeInfo =
    codemirror.modeInfo
      ? codemirror.modeInfo.push(mimeType)
      : [mimeType]

  return codemirror;
};

if (window.CodeMirror) {
  initAbapMode(window.CodeMirror);
}
