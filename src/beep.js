var bell = new Audio(`bell.mp3`);

var app = Elm.Main.init();

app.ports.beep.subscribe(
  function(){
    bell.play();
  }
);
