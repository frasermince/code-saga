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
      codeTagProps: {style: {paddingTop: "10px", display: 'block', overflowY: 'scroll'}},
      lineNumberContainerStyle: { textAlign: 'right',  padding: '10px', float: 'left', backgroundColor: '#fafafa', boxSizing: 'border-box', width: '38px' },
      lineNumberStyle: { color: 'rgba(0,0,0,0.3)', fontSize: '12px', lineHeight: '19px' },
      customStyle: { marginBottom: 0, marginLeft: 0, padding: 0, boxSizing: 'border-box' },
      lineStyle: function (lineNumber) {
        var style = { display: 'block', paddingLeft: '10px', width: '100%', minHeight: '19px'};
        if (lineNumber === highlightedLineNumber) {
          style.backgroundColor = '#dbffdb';
        }
        return style;
      },
      style: styles.githubGist
    }, this.props.content);
  }
});
