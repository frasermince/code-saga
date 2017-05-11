const appConfig = require('./src/App/Config.js').config
const webpack = require('webpack')
const spawn = require('child_process').spawn

const path = require('path')
const entries = [path.join(__dirname, 'support/test.entry.js')]
const plugins = [
  new webpack.ProvidePlugin({
    'XMLHttpRequest': 'xhr2'
  }),
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  })
]

const config = {
  entry: entries,
  target: 'node',
  output: {
    path: path.join(__dirname, 'static', 'dist'),
    filename: 'test.js',
    publicPath: '/',
    libraryTarget: 'commonjs2'
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
          psc: 'psa',
          pscIde: true,
          src: [
            path.join('src', '**', '*.purs'),
            path.join('bower_components', 'purescript-*', 'src', '**', '*.purs'),
            path.join('test', '**', '*.purs'),
          ]
        }
      }
    ],
  },
  plugins: plugins,
  resolve: {
    alias: {
      'react': 'preact-compat',
      'react-dom': 'preact-compat'
    },
    modules: [
      'node_modules'
    ],
    extensions: ['.js', '.purs']
  }
}

if (require.main === module) {
  const compiler = webpack(config);
  const client = webpack(require('./webpack.config.client.js'));
  let server = null;
  const app = require('express')();
  app.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    next();
  });
  app.use(require('webpack-dev-middleware')(client, {
    noInfo: true,
    publicPath: appConfig.public_path
  }));
  app.use(require('webpack-hot-middleware')(client));
  app.listen(8080);

  console.log('Compiling...')

  compiler.watch({
    aggregateTimeout: 300,
    poll: undefined
  }, (err, stats) => {

    if (err) return console.error(err)

    response = require('./static/dist/test.js');
    // console.log('response', response);
    // if (server && !stats.hasErrors()) {
      // server.kill('SIGKILL')
      // server = spawn('node', ['./dist/server.js']);
    // } else {
      // spawn('node', ['./static/dist/test.js']);
      // server = spawn('node', ['./dist/server.js']);
    // }
  })
} else {
  module.exports = config
}
