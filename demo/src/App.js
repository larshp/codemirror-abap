import React from 'react';
import './App.scss';
import { UnControlled as CodeMirrorControl } from 'react-codemirror2';
import { abapMode, initAbapMode } from 'codemirror-mode-abap/lib/index';
import CodeMirror from 'codemirror';
import 'codemirror/addon/display/fullscreen';

function App() {
  initAbapMode(CodeMirror);
  return (
    <div className="App">
      <CodeMirrorControl
        value={`* codemirror-mode-abap: playground
WRITE 'Hello world!'.`}
        options={{
          theme: 'material-darker',
          lineNumbers: true,
          tabSize: 2,
          mode: 'abap',
          matchBrackets: true,
          fullScreen: true,
        }}
        onChange={(editor, data, value) => {}}
      />
    </div>
  );
}

export default App;
