// Calculator

function Calculator() {
}

Calculator.prototype = new PstNode();

Calculator.prototype.updateDisplayValue = function() {
  var newValue = this.newValue.get();
  var operand = this.operand.get();
  this.displayValue.set(newValue != NULL ? newValue : operand);
};

Calculator.CalculatorKey = function() {
  this.width = "2em";
};

Calculator.CalculatorKey.prototype = new Button();

Calculator.prototype.calculate = function() {
  var op = this.op.get();
  var operand = this.operand.get();
  var newValue = this.newValue.get();
  this.operand.set((op == 1 ? function(a, b) { return a + b; } :
                    op == 2 ? function(a, b) { return a - b; } :
                    op == 3 ? function(a, b) { return a * b; } :
                    op == 4 ? function(a, b) { return a / b; } :
                    error())
                   (operand, newValue));
  this.newValue.set(NULL);
};

Calculator.NumKey = function(env) {
  this.env = env;
  this.num = NULL;
};

Calculator.NumKey.prototype = new PstNode();

Calculator.NumKey.prototype.create = function(domParent) {
  var calculatorKey = new Calculator.CalculatorKey();
  calculatorKey.label = "" + this.num;
  calculatorKey.color = "#dedede";
  var This = this;
  calculatorKey.onEvent = function(event, props) {
    if (props.type == "uiClicked") {
      var newValue = This.env.newValue.get();
      This.env.newValue.set((10 * (newValue != NULL ? newValue : 0)) + This.num);
    }
  };
  calculatorKey.create(domParent);
  calculatorKey.parent = this;
};

Calculator.OpKey = function(env) {
  this.env = env;
  this.op = NULL; // TODO: NULL => undefined
};

Calculator.OpKey.prototype = new PstNode();

Calculator.OpKey.prototype.create = function(domParent) {
  var calculatorKey = new Calculator.CalculatorKey();
  calculatorKey.label = (this.op == 1 ? "+" :
                         this.op == 2 ? "-" :
                         this.op == 3 ? "*" :
                         this.op == 4 ? "/" :
                         error());
  calculatorKey.color = "#aaffdd";
  var This = this;
  calculatorKey.onEvent = function(event, props) {
    if (props.type == "uiClicked") {
      var op = This.env.op.get();
      var newValue = This.env.newValue.get();
      if (op != NULL) {
        This.env.calculate();
      } else {
        This.env.operand.set(newValue);
      }
      This.env.op.set(This.op);
      This.env.newValue.set(NULL);
    }
  };
  calculatorKey.create(domParent);
  calculatorKey.parent = this;
};

Calculator.EnterKey = function(env) {
  this.env = env;
};

Calculator.EnterKey.prototype = new PstNode();

Calculator.EnterKey.prototype.create = function(domParent) {
  var calculatorKey = new Calculator.CalculatorKey();
  calculatorKey.label = "=";
  calculatorKey.color = "#aaffdd";
  var This = this;
  calculatorKey.onEvent = function(event, props) {
    if (props.type == "uiClicked") {
      This.env.calculate();
      This.env.op.set(NULL);
    }
  };
  calculatorKey.create(domParent);
  calculatorKey.parent = this;
};

Calculator.prototype.create = function(domParent) {

  var This = this;

  this.operand = new IntegerType().createValue(0);
  this.newValue = new IntegerType().createValue(NULL);
  this.op = new IntegerType().createValue(NULL); // :+ => 1  :- => 2  :* => 3  :/ => 4
  this.displayValue = new IntegerType().createValue(0);

  var display = this.display = new Text();
  display.id = "display";
  display.value = this.displayValue.get();

  var newValueEventSink = this.newValueEventSink = new EventSink(this.newValue);
  newValueEventSink.afterUpdate = function(_, x) { This.updateDisplayValue(); };

  var operandEventSink = this.operandEventSink = new EventSink(this.operand);
  operandEventSink.afterUpdate = function(_, x) { This.updateDisplayValue(); };

  var displayValueEventSink = this.displayValueEVentSink = new EventSink(this.displayValue);
  displayValueEventSink.afterUpdate = function(_, x) { This.display.setValue(x); };

  var _0 = new Calculator.NumKey(this); // TODO: pass 'id' in the constructor.
  _0.id = "0";
  _0.num = 0;

  var _1 = new Calculator.NumKey(this);
  _1.id = "1";
  _1.num = 1;

  var _2 = new Calculator.NumKey(this);
  _2.id = "2";
  _2.num = 2;

  var _3 = new Calculator.NumKey(this);
  _3.id = "3";
  _3.num = 3;

  var _4 = new Calculator.NumKey(this);
  _4.id = "4";
  _4.num = 4;

  var _5 = new Calculator.NumKey(this);
  _5.id = "5";
  _5.num = 5;

  var _6 = new Calculator.NumKey(this);
  _6.id = "6";
  _6.num = 6;

  var _7 = new Calculator.NumKey(this);
  _7.id = "7";
  _7.num = 7;

  var _8 = new Calculator.NumKey(this);
  _8.id = "8";
  _8.num = 8;

  var _9 = new Calculator.NumKey(this);
  _9.id = "9";
  _9.num = 9;

  var _PLUS = new Calculator.OpKey(this);
  _PLUS.id = "+";
  _PLUS.op = 1;

  var _MINUS = new Calculator.OpKey(this);
  _MINUS.id = "-";
  _MINUS.op = 2;

  var _TIMES = new Calculator.OpKey(this);
  _TIMES.id = "*";
  _TIMES.op = 3;

  var _DIVIDE = new Calculator.OpKey(this);
  _DIVIDE.id = "/";
  _DIVIDE.op = 4;

  var enterKey = new Calculator.EnterKey(this);
  enterKey.id = "enter-key";

  var grid = this.grid = new Grid();
  var grid_row0 = { children: [_7, _8, _9, _DIVIDE] };
  var grid_row1 = { children: [_4, _5, _6, _TIMES] };
  var grid_row2 = { children: [_1, _2, _3, _MINUS] };
  var grid_row3 = { children: [_0, null, enterKey, _PLUS] };
  grid.rows = [grid_row0, grid_row1, grid_row2, grid_row3];

  var verticalLayout = this.verticalLayout = new VerticalLayout();
  verticalLayout.children = [display, grid];
  verticalLayout.create(domParent);
  verticalLayout.parent = this;
};


// main

function onLoad() {
  var calculator = new Calculator();
  calculator.create(document.body);
}
