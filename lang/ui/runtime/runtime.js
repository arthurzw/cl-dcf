//
// Null value. This way, we don't confuse JavaScript null and 0.
//

var NULL = new Object();


// Built-in functions

function reduce(arr, init, lambda) {
  var val = init;
  for (var i = 0; i < arr.length; ++i) {
    val = lambda.call(null, val, arr[i]);
  }
  return val;
}

function add() { return reduce(arguments, 0, function(x, y) { return x + y; }); }
function subtract() { return reduce(arguments, 0, function(x, y) { return x + y; }); }
function multiply() { return reduce(arguments, 1, function(x, y) { return x + y; }); }

function divide() {
  if (arguments.length == 1) {
    return 1 / arguments[0];
  } else {
    var val = arguments[0];
    for (var i = 1; i < arguments.length; ++i) {
      val = val / arguments[i];
    }
    return val;
  }
}


// PST Node

function PstNode () {
  this.parent = null;
  this.id = null;
  this.value = null;
}

PstNode.prototype.onEvent = function(event, props) {
  if (this.parent) {
    this.parent.onEvent(event, props);
  }
};


// Button.

function Button() {
  this.label = "";
  this.width = null;
  this.color = null;
}

Button.prototype = new PstNode();

Button.prototype.create = function(domParent) {
  var input = this.domNode = document.createElement("input");
  input.type = "button";
  input.id = this.id;
  if (this.label) input.value = this.label;
  if (this.width) input.style.width = this.width;
  if (this.color) input.style.backgroundColor = this.color;
  var self = this;
  input.onclick = function(event) {
    self.onEvent(event, { uiWidget: self.id, type: "uiClicked" });
  };
  domParent.appendChild(input);
};


// Checkbox.

function Checkbox() {
  this.label = "";
  this.valueDataBinding = null;
}

Checkbox.prototype = new PstNode();

Checkbox.prototype.create = function(domParent) {
  var span = this.domNode = document.createElement("span");
  var input = this.input = document.createElement("input");
  var textNode = this.textNode = document.createTextNode("");
  span.appendChild(input);
  span.appendChild(textNode);
  input.type = "checkbox";
  input.id = this.id;
  input.style.marginRight = "0.3em";
  if (this.label) textNode.nodeValue = this.label;
  if (this.valueDataBinding) input.checked = this.valueDataBinding.get(); // TODO: make it work for constants.
  var self = this;
  input.onclick = function(event) {
    if (self.valueDataBinding) self.valueDataBinding.set(input.checked);
  };
  // TODO: make the text node clickable--onclick event handler + cursor style. Hot-tracking?
  domParent.appendChild(span);
};

Checkbox.prototype.setLabel = function(label) {
  this.label = label;
  this.textNode.nodeValue = label;
};


// Text

function Text() {
  this.value = "";
}

Text.prototype = new PstNode();

Text.prototype.create = function(domParent) {
  var span = this.domNode = document.createElement("span");
  span.id = this.id;
  span.appendChild(document.createTextNode(this.value));
  domParent.appendChild(span);
};

Text.prototype.setValue = function(value) {
  this.value = value;
  this.domNode.firstChild.data = value;
};


// HorizontalLayout

function HorizontalLayout() {
  this.children = [];
}

HorizontalLayout.prototype = new PstNode();

HorizontalLayout.prototype.create = function(domParent) {
  var table = this.domNode = document.createElement("table");
  var tr = document.createElement("tr");
  table.appendChild(tr);
  for (var i = 0; i < this.children.length; ++i) {
    var child = this.children[i];
    var td = document.createElement("td");
    tr.appendChild(td);
    child.create(td);
    child.parent = this;
  }
  domParent.appendChild(table);
};


// VerticalLayout

function VerticalLayout() {
  this.children = [];
}

VerticalLayout.prototype = new PstNode();

VerticalLayout.prototype.create = function(domParent) {
  var table = this.domNode = document.createElement("table");
  for (var i = 0; i < this.children.length; ++i) {
    var child = this.children[i];
    var tr = document.createElement("tr");
    table.appendChild(tr);
    var td = document.createElement("td");
    tr.appendChild(td);
    child.create(td);
    child.parent = this;
  }
  domParent.appendChild(table);
};


// Grid

function Grid() {
  this.rows = [];
}

Grid.prototype = new PstNode();

Grid.prototype.create = function(domParent) {

  var table = this.domNode = document.createElement("table");
  for (var i = 0; i < this.rows.length; ++i) {
    var row = this.rows[i];
    var tr = document.createElement("tr");
    table.appendChild(tr);

    for (var j = 0; j < row.children.length; ++j) {
      var child = row.children[j];
      var td = document.createElement("td");
      tr.appendChild(td);
      if (child) {
        child.create(td);
        child.parent = this;
      }
    }

    domParent.appendChild(table);
  }
};
