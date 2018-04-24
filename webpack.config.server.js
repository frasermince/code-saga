const path = require('path')
const webpack = require('webpack')
const nodeExternals = require('webpack-node-externals')
const isProd = process.env.NODE_ENV === 'production'

const entries = [path.join(__dirname, 'support', 'server.entry.js')]
var plugins = [
  new webpack.ProvidePlugin({
    'XMLHttpRequest': 'xhr2'
  }),
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
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
}

module.exports = {
  entry: entries,
  target: 'node',
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'server.js',
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
            pscPackage: true
        }
      },
      {
        test: /\.(eot|svg|ttf|woff|woff2)$/,
        loader: 'file?name=static/dist/[name].[ext]'
      }
    ],
  },
  plugins: plugins,
  externals: [nodeExternals({
    whitelist: ['XMLHttpRequest', 'webpack/hot/poll?1000'],
  })],
  resolve: {
    alias: {
      'react': 'preact-compat',
      'react-dom': 'preact-compat',
      'create-react-class': 'preact-compat/lib/create-react-class'
    },
    modules: [
      'node_modules'
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
