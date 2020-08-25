<p align="center">
  <img height="150" src="img/logo.svg">
</p>
<h1 align="center"> codemirror-abap </h1>
<p align="center">
  <b>ABAP mode for the CodeMirror editor</b>
</p>

[![NPM Version][npm-badge]][npm-url]
[![Github Actions][gh-actions-badge]][gh-actions-url]
[![Codecov][codecov-badge]][codecov-url]
[![Dependency Status][dependency-badge]][dependency-url]

## Description

Add ABAP syntax highlighting to the [CodeMirror][codemirror-url] editor.

## Installation

``` bash
npm install --save codemirror-abap
```

## Usage

### Build tools

1. Import `codemirror-abap` in your project.  

    ```js
    import CodeMirror from 'codemirror'
    import { initAbapMode } from 'codemirror-abap/lib/index'

    // register Mode and MIME type
    initAbapMode(CodeMirror);
    ```

    or 

    ```js
    import CodeMirror from 'codemirror'
    import { abapMode } from 'codemirror-abap/lib/index'

    // explicitly register mode
    CodeMirror.defineMode("abap", abapMode);
    ```

2. Set 'abap' as the mode when instantiating the CodeMirror editor.

    ```js
    CodeMirror.fromTextArea(element, {
        mode: 'abap'
    });
    ```

### Browser

1. Include the `codemirror-abap` bundle in a script tag.  

    ```html
    <!-- CodeMirror is a peer dependency -->
    <script src="js/codemirror.js"></script>
    <!-- Mode and MIME type are register automatically  -->
    <script src="js/codemirror-abap/bundles/abap.min.js"></script>
    ```

2. Set 'abap' as the mode when instantiating the CodeMirror editor.

    ```js
    CodeMirror.fromTextArea(element, {
        mode: 'abap'
    });
    ```

[npm-badge]: https://img.shields.io/npm/v/codemirror-abap.svg
[npm-url]: https://www.npmjs.com/package/codemirror-abap
[gh-actions-badge]: https://img.shields.io/github/workflow/status/larshp/codemirror-abap/test
[gh-actions-url]: https://github.com/larshp/codemirror-abap/actions
[codecov-badge]: https://codecov.io/gh/larshp/codemirror-abap/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/larshp/codemirror-abap
[dependency-badge]: https://david-dm.org/larshp/codemirror-abap.svg
[dependency-url]: https://david-dm.org/larshp/codemirror-abap
[codemirror-url]: https://github.com/codemirror/CodeMirror
