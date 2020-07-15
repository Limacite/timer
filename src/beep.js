var bell = new Audio(`bell.mp3`);

var app = Elm.Main.init({
  node: document.getElementById(`elm`)
});

app.ports.beep.subscribe(
  function(){
    bell.play();
  }
);
