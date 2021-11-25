<p align="center">
  <img height="250" src="img/logo.svg">
</p>
<h1 align="center"> codemirror-abap </h1>
<p align="center">
  <b>ABAP mode for the CodeMirror editor</b>
</p>

[![NPM Version][npm-badge]][npm-url]
[![Github Actions][gh-actions-badge]][gh-actions-url]
[![Codecov][codecov-badge]][codecov-url]

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
    import CodeMirror from 'codemirror';
    // mode and MIME type are registered automatically
    import 'codemirror-abap';
    ```

    or 

    ```js
    import CodeMirror from 'codemirror';
    import { abapMode } from 'codemirror-abap';

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
    <!-- mode and MIME type are registered automatically  -->
    <script src="js/codemirror-abap/bundles/abap.min.js"></script>
    ```

    or

    ```html
    <script src="js/codemirror.js"></script>
    <script src="js/codemirror-abap/bundles/abap.min.js"></script>

    <!-- explicitly register mode  -->
    CodeMirror.defineMode("abap", abap.abapMode);
    ```

2. Set 'abap' as the mode when instantiating the CodeMirror editor.

    ```js
    CodeMirror.fromTextArea(element, {
        mode: 'abap'
    });
    ```

### CDN
```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.57.0/codemirror.min.js"></script>
<script src="https://cdn.jsdelivr.net/npm/codemirror-abap/bundles/abap.min.js"></script>
```

## Demo

There's a simple React demo using [react-codemirror2](https://www.npmjs.com/package/react-codemirror2) in the [demo](/demo) directory of the git repo.
To run the demo:

```bash
git clone https://github.com/larshp/codemirror-abap.git
cd codemirror-abap/demo
npm install
npm start
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
