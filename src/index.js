// pull in desired CSS/SASS files
require( './styles/main.scss' );

var Howl = require('howler').Howl;

// inject bundled Elm app into div#main
var Elm = require( './Main' );
const config = localStorage.getItem('config');
const startingConfig = config ? JSON.parse(config) : null;
var app = Elm.Main.embed( document.getElementById( 'main' ), startingConfig);

var sounds = {};
app.ports.preload.subscribe(function(files){
  files.forEach(function(path){
    sounds[path] = new Howl({src:[path], preload:true});
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
})
