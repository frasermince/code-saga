var styles = require('react-syntax-highlighter/dist/styles/');
var highlighter = require('react-syntax-highlighter/dist/light');
var ruby = require('react-syntax-highlighter/dist/languages/ruby');
var React = require('react');
var createReactClass = require('create-react-class');

highlighter.registerLanguage('ruby', ruby.default);
exports.highlightClass = createReactClass({
  render: function () {
    var highlightedLineNumber = this.props.lineNumber;
    return React.createElement(highlighter.default, {
      wrapLines: true,
      language: 'ruby',
      showLineNumbers: true,
      lineStyle: function (lineNumber) {
        var style = { display: 'block' };
        if (lineNumber === highlightedLineNumber) {
          style.backgroundColor = '#dbffdb';
        }
        return style;
      },
      style: styles.githubGist
    }, this.props.content);
  }
});
