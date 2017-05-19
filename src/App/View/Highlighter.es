import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco } from 'react-syntax-highlighter/dist/styles';
var React = require('react');
var createReactClass = require('create-react-class');

exports.highlightClass = class HighlightClass extends React.Component{
  render(){
    const codeString = '(num) => num + 1';
    return React.createElement(SyntaxHighlighter, {
      language: 'ruby',
      style: docco
    }, this.props.content);
  }
};
