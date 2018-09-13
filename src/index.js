import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const config = localStorage.getItem('config');

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: config? JSON.parse(config) : null
});

registerServiceWorker();

var sounds = {};
app.ports.preload.subscribe(function(files){
  files.forEach(function(path){
    sounds[path] = new Audio(path);
  });
});

app.ports.play.subscribe(function(paths) {
  paths.forEach(function(p) {
    var f = sounds[p];
    if(f){
      f.play();
    }
  })
});

app.ports.saveConfig.subscribe(function(config) {
  console.log('saveconfig', config);
  localStorage.setItem('config', JSON.stringify(config));
});
