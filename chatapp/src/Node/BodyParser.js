"use strict";

const bodyParser = require("body-parser");

exports.json = bodyParser.json({
    limit: "5mb"
});
