var reactDOMServer = require('react-dom/server');

exports.renderToString = function(jsx) {
  return reactDOMServer.renderToString(jsx);
};
