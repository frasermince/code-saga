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
      codeTagProps: {style: {paddingTop: "10px", display: 'block', flexGrow: 100, minHeight: '100%' }},
      lineNumberContainerStyle: { textAlign: 'right',  padding: '10px', backgroundColor: '#fafafa', boxSizing: 'border-box', minHeight: '100%' },
      lineNumberStyle: { color: 'rgba(0,0,0,0.3)', fontSize: '12px', lineHeight: '19px', width: '38px', display: 'block', flex: 1 },
      customStyle: { marginBottom: 0, marginLeft: 0, marginTop: 0, padding: 0, boxSizing: 'border-box', position: 'relative', overflowY: 'scroll', display: 'flex', alignItems: 'flex-start' },
      lineStyle: function (lineNumber) {
        var style = { display: 'inline-block', paddingLeft: '10px', paddingRight: '10px', minWidth: '100%', minHeight: '19px', clear: 'both', float: 'left'};
        if (lineNumber === highlightedLineNumber) {
          style.backgroundColor = '#fffbdd';
        }
        return style;
      },
      style: styles.githubGist
    }, this.props.content);
  }
});
