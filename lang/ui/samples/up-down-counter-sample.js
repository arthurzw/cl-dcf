// UpDownCounterSample

function UpDownCounterSample() {

  var counterValue = this.counterValue = new IntegerType().createValue(0);

  var display = this.display = new Text();
  display.id = "display";
  display.value = counterValue.get();

  var displayEventSink = this.displayEventSink = new EventSink(counterValue);
  displayEventSink.afterUpdate = function(oldX, x) { display.setValue(x); };

  var up = this.up = new Button();
  up.id = "up";
  up.label = "Up";

  var down = this.down = new Button();
  down.id = "down";
  down.label = "Down";

  var reset = this.reset = new Button();
  reset.id = "reset";
  reset.label = "Reset";
  reset.onEvent = function(event, props) {
    if (props.type == "ui:clicked") {
      counterValue.set(0);
    }
  };

  var horizontalLayout = this.horizontalLayout = new HorizontalLayout();
  horizontalLayout.children = [display, up, down, reset];
}

UpDownCounterSample.prototype = new PstNode();

UpDownCounterSample.prototype.create = function(domParent) {
  this.horizontalLayout.create(domParent);
  this.horizontalLayout.parent = this;
};

UpDownCounterSample.prototype.onEvent = function(event, props) {
  if (props.ui_widget == "up" && props.type == "uiClicked") {
    this.counterValue.set(this.counterValue.get() + 1);
  } else if (props.ui_widget == "down" && props.type == "uiClicked") {
    this.counterValue.set(this.counterValue.get() - 1);
  } else {
    // (log "This illustrates a catch-all handler.")
  }
};


// main

function onLoad() {
  var upDownCounterSample = new UpDownCounterSample();
  upDownCounterSample.create(document.body);
}
