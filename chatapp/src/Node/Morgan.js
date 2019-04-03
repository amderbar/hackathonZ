'use strict';

const morgan = require('morgan');

exports.logger = function (format) {
    return morgan(format);
};
