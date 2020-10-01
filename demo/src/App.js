import React from 'react';
import './App.scss';
import { UnControlled as CodeMirrorControl } from 'react-codemirror2';
import CodeMirror from 'codemirror';
import 'codemirror/addon/display/fullscreen';
import 'codemirror-abap';

function App() {
  return (
    <div className="App">
      <CodeMirrorControl
        value={`* codemirror-abap: playground
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
