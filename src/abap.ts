import CodeMirror from 'codemirror';
import 'codemirror/addon/runmode/runmode';
import { KEYWORDS, OPERATORS, COMMENT, STRING, NUMBER, KEYWORD, OPERATOR } from "./constants"

class State {
  public mode: boolean;
}

class AbapMode implements CodeMirror.Mode<State> {

  public startState: () => State;

  private keywords;

  public constructor() {
    this.startState = () => { return new State(); };
    this.setupKeywords();
  }

  public token(stream: CodeMirror.StringStream, state: State) {

    if (stream.eatSpace()) {
      return undefined;
    }

    if (this.isKeyword(stream)) {
      return KEYWORD;
    } else if (stream.match(/^\d+( |\.|$)/, false)) {
      stream.match(/^\d+/);
      return NUMBER;
    } else if (stream.match(/^##\w+/)) {
      // pragmas
      return COMMENT;
    }

    const ch = stream.next();
    let peek = stream.peek();
    if (peek === undefined) {
      peek = "";
    }

    if ((ch === "*" && stream.column() === 0) || ch === '"') {
      stream.skipToEnd();
      return COMMENT;
    } else if (this.isOperator(stream)) {
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
      return null;
    }
  };

  private setupKeywords() {


    this.keywords = KEYWORDS
      .reduce(
        (result, word) => ({
          ...result,
          [word]: true,
        }),
        {},
      );
  }

  private checkMatch(stream: CodeMirror.StringStream, separators: string | string[], callback): boolean {
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

  private isOperator(stream: CodeMirror.StringStream): boolean {
    const checkOperator = (input: string) => OPERATORS.includes(input);
    return this.checkMatch(stream, " ", checkOperator)
  }

  private isKeyword(stream: CodeMirror.StringStream): boolean {
    const checkKeyword = (input: string) =>
      this.keywords.propertyIsEnumerable(input);
    const KEYWORD_SEPARATORS = "(.,: ";

    return this.checkMatch(stream, KEYWORD_SEPARATORS, checkKeyword)
  }
}

export function ABAPFactory(options: CodeMirror.EditorConfiguration, spec: State): CodeMirror.Mode<State> {
  return new AbapMode();
}

export function initAbapMode(codemirror: any) {
  codemirror.defineMode('abap', ABAPFactory);
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
