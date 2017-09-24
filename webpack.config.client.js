const appConfig = require('./src/App/Config.js').config
const spawn = require('child_process').spawn
const path = require('path')
const webpack = require('webpack')
const isProd = process.env.NODE_ENV === 'production'
const CompressionPlugin = require("compression-webpack-plugin");
const UglifyJSPlugin = require('uglifyjs-webpack-plugin');

const entries = [path.join(__dirname, 'support/client.entry.js')]

var plugins = [
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  }),
  new CompressionPlugin({
    asset: "[path].gz[query]",
    algorithm: "gzip",
    test: /\.(js|html)$/,
    threshold: 10240,
    minRatio: 0.8
  })
]

if (isProd) {
  plugins = plugins.concat([
    // new webpack.LoaderOptionsPlugin({
    //   minimize: true,
    //   debug: false
    // }),
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        warnings: false,
        unused: true,
        dead_code: true,
      },
      output: {
        comments: false,
      }
    })
  ])
  // console.log("plugins", plugins)
} else {
  entries.unshift('webpack-hot-middleware/client?path=http://localhost:8080/__webpack_hmr&reload=true');
  plugins.push(
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoEmitOnErrorsPlugin()
  )
}

const config = {
  entry: entries,
  context: __dirname,
  target: 'web',
  output: {
    path: path.join(__dirname, 'static', 'dist'),
    filename: 'bundle.js',
    publicPath: appConfig.public_path
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: isProd ? {
          bundle: true,
          bundleOutput: 'static/dist/bundle.js'
        } : {
          psc: 'psa',
          pscIde: true
        }
      },
      {
        test: /\.(eot|svg|ttf|woff|woff2)$/,
        loader: 'file?name=static/dist/[name].[ext]'
      }
    ],
  },
  plugins: plugins,
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  },
  resolve: {
    alias: {
      'react': 'preact-compat',
      'react-dom': 'preact-compat',
      'create-react-class': 'preact-compat/lib/create-react-class'
    },
    modules: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['.js', '.purs']
  },
  performance: { hints: false },
  stats: {
    hash: false,
    timings: false,
    version: false,
    assets: false,
    errors: true,
    colors: false,
    chunks: false,
    children: false,
    cached: false,
    modules: false,
    chunkModules: false
  },
  node: {
    fs: 'empty'
  }
}

// If this file is directly run with node, start the development server
// instead of exporting the webpack config.
if (require.main === module) {
  const compiler = webpack(require('./webpack.config.server.js'));
  const client = webpack(config);
  let server = null;
  const app = require('express')();
  app.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    next();
  });
  app.use(require('webpack-dev-middleware')(client, {
    noInfo: true,
    publicPath: config.output.publicPath
  }));
  app.use(require('webpack-hot-middleware')(client));
  app.listen(8080);

  console.log('Compiling...');

  compiler.watch({
    aggregateTimeout: 300,
    poll: 1000
  }, (err, stats) => {
    if (err) return console.error("***ERROR: ", err);

    if (server && !stats.hasErrors()) {
      server.kill('SIGKILL');
      server = spawn('node', ['./dist/server.js']);
    } else {
      server = spawn('node', ['./dist/server.js']);
    }
    server.stdout.on('data', function(data) {
      // Here is where the output goes
      console.log('stdout: ' + data);
    });
    server.stderr.on('data', function(data) {
      // Here is where the error output goes
      console.log('stderr: ' + data);
    });
    server.on('close', function(code) {
      // Here you can get the exit code of the script
      console.log('closing code: ' + code);
    });
  });
} else {
  module.exports = config;
}
