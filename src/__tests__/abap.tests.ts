import CodeMirror from 'codemirror';
import { initAbapMode } from '../abap';
import { COMMENT, KEYWORD, NUMBER, OPERATOR, STRING } from '../constants';

expect(initAbapMode(CodeMirror)).toEqual(CodeMirror);

const syntax = (code: string) => {
  let res = '';
  CodeMirror.runMode(code, 'abap', (text, style) => {
    if (style == null) {
      return; // skip space
    } else if (res.length === 0) {
      res = style;
    } else {
      res = res + ' ' + style;
    }
  });
  return res;
};

const combineTypes = (...types: string[]) => types.join(' ');

const testType = (input: string, type: string) =>
  test(`${type.toLowerCase()}: '${input}'`, () =>
    expect(syntax(input)).toEqual(type));

const testComment = (input: string) => testType(input, COMMENT);
const testKeyword = (input: string) => testType(input, KEYWORD);
const testNumber = (input: string) => testType(input, NUMBER);
const testOperator = (input: string) => testType(input, OPERATOR);
const testString = (input: string) => testType(input, STRING);

describe('commments', () => {
  testComment(`* comment`);
  testComment(`" comment`);
  testComment(` " comment`);
  testComment('##PRAGMA');
});

describe('keywords', () => {
  testKeyword(`endclass`);
  testKeyword(`type`);
  testKeyword(`DATA(`);
  testKeyword(`function`);
});

describe('numbers', () => {
  testNumber('1');
  testNumber('12345');
});

describe('operators', () => {
  testOperator('=');
  testOperator('<>');
  testOperator('GT');
  testOperator('?=');
});

describe('strings', () => {
  const TEST_HELLO_WORLD = 'Hello World!';
  testString(`'${TEST_HELLO_WORLD}'`);
  testString(`|${TEST_HELLO_WORLD}|`);
});

describe('combinations', () => {
  testType(`write 'hello world'`, combineTypes(KEYWORD, STRING));
  testType(`IF 2 > 1`, combineTypes(KEYWORD, NUMBER, OPERATOR, NUMBER));
  testType(`DATA(lv_test) = abap_false.`, combineTypes(KEYWORD, OPERATOR));
  testType(
    `DATA lv_text TYPE string ##NEEDED.`,
    combineTypes(KEYWORD, KEYWORD, COMMENT),
  );
  testType(
    `DATA(lv_matnr) = VALUE #( iv_matnr ).`,
    combineTypes(KEYWORD, OPERATOR, KEYWORD),
  );
});
