function createElementEx(elementName, opt_className, opt_text) {
  var element = document.createElement(elementName);
  if (opt_className) {
    element.className = opt_className;
  }
  if (opt_text) {
    element.appendChild(document.createTextNode(opt_text));
  }
  return element;
}


function createDiv(opt_className, opt_text) {
  return createElementEx("div", opt_className, opt_text);
}


function createSpan(opt_className, opt_text) {
  return createElementEx("span", opt_className, opt_text);
}


function createLabelSpan(opt_className, label, text) {
  var span = createSpan(opt_className);
  var labelSpan = createSpan("label", label);
  span.appendChild(labelSpan);
  span.appendChild(document.createTextNode(text));
  return span;
}


function createInputText(opt_className, opt_text) {
  var element = createElementEx("input", opt_className, opt_className);
  element.type = "text";
  if (opt_text) {
    element.value = opt_text;
  }
  return element;
}


function createInputButton(opt_className, opt_text) {
  var element = createElementEx("input", opt_className, opt_className);
  element.type = "button";
  if (opt_text) {
    element.value = opt_text;
  }
  return element;
}

