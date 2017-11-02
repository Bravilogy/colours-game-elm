'use strict';

require('./scss/app.scss');

require('./index.html');

const Elm = require('./elm/Main.elm');
const app = Elm.Main.embed(document.getElementById('main'));
