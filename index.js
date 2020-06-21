import { Elm } from './src/Main.elm';
import 'normalize.css';
import 'animate.css';

// Styles
require('./assets/styles/skeleton.css');
require('./assets/styles/main.scss');

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: []
});
