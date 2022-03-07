const path = require('path');

module.exports = {
  mode: 'production',
  entry: {
    abap: path.resolve(__dirname, '../src/abap.ts'),
    'abap.min': path.resolve(__dirname, '../src/abap.ts'),
  },
  output: {
    path: path.resolve(__dirname, '../bundles'),
    filename: '[name].js',
    libraryTarget: 'umd',
    library: 'abap',
    umdNamedDefine: true,
  },
  externals: ['codemirror', /^codemirror\/.+$/],
  resolve: {
    extensions: ['.ts', '.tsx', '.js'],
  },
  devtool: 'source-map',
  optimization: {
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loader: 'ts-loader',
        options: {
          configFile: path.resolve(__dirname, '../config/tsconfig.json'),
        },
        exclude: /node_modules/,
      },
    ],
  },
};
