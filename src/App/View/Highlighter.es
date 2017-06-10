import { githubGist } from 'react-syntax-highlighter/dist/styles/';
import SyntaxHighlighter, { registerLanguage } from "react-syntax-highlighter/dist/light"
import rb from 'react-syntax-highlighter/dist/languages/ruby';
var React = require('react');
var createReactClass = require('create-react-class');

registerLanguage('ruby', rb);
exports.highlightClass = class HighlightClass extends React.Component{
  render(){
    const codeString = '(num) => num + 1';
    return React.createElement(SyntaxHighlighter, {
      showLineNumbers: true,
      language: 'ruby',
      style: githubGist
    }, this.props.content);
  }
};
