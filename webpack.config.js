/* jshint esversion: 6 */
/* jshint node: true */
/* jshint -W097 */

'use strict';

const path = require('path');
const webpack = require('webpack');

module.exports = {
  mode: 'development',
  entry: './src/frontend.js',
  output: {
    pathinfo: true,
    filename: 'bundle.js'
  },
  resolve: {
    modules: ['node_modules', ...process.env.NODE_PATH.split(':')]
  },
  devServer: {
    index: './index.html',
    publicPath: '/static',
    contentBase: './dist',
    contentBasePublicPath: '/static',
  },
};
