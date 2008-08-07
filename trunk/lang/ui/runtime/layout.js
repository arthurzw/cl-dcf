//
// Layout library. Sample usage:
//
//   hl = createHorizontalLayout([
//     { box: createDiv("my-box", "leftmost"), size: 4 },
//     { box: createDiv("my-box", "left-area"), flex: 1, minSize: 5 },
//     { box: createDiv("my-box", "middle"), size: 4 },
//     { box: createDiv("my-box", "right-area"), flex: 2, minSize: 5 },
//     { box: createDiv("my-box", "rightmost"), size: 6 }
//   ], 0.5);
//
//   vl = createVerticalLayout([
//     { box: createDiv("my-box", "header"), size: 1.5 },
//     { box: hl, flex: 1, minSize: 5 },
//     { box: createDiv("my-box", "middle"), size: 1.5 },
//     { box: createDiv("my-box", "lower-area"), flex: 2, minSize: 5 },
//     { box: createDiv("my-box", "footer"), size: 1.5 }
//   ], 0.5);
//

function prepareLayout(panels) {

  var flexCount = 0;
  for (var i = 0; i < panels.length; ++i) {
    var panel = panels[i];
    if (panel.flex) {
      flexCount += panel.flex;
    }
  }

  return flexCount;
}


function createVerticalLayout(panels, opt_spacing) {

  var flexCount = prepareLayout(panels);

  var layoutDiv = createDiv();
  layoutDiv.setAttribute("metaclass", "layout");
  layoutDiv.setAttribute("layoutDirection", "vertical");
  var flexFrame = createDiv("flex-frame");
  var fixedSize = 0;
  var flexBlock = null;

  function createPanel(panel) {

    if (!flexBlock) {
      flexBlock = createDiv("flex-block");
      flexFrame.appendChild(flexBlock);
    }

    var container = createDiv("container");
    flexBlock.appendChild(container);

    var box = createDiv("box");
    box.style.top = fixedSize + "em";
    if (panel.box) {
      box.appendChild(panel.box);
    }
    container.appendChild(box);

    if (panel.flex) {
      flexBlock.style.height = (100 * panel.flex / flexCount) + "%";
      if (panel.minSize) {
        flexBlock.style.minHeight = panel.minSize + "em";
      }
      box.style.height = "100%";
      flexBlock = null;
    } else {
      box.style.height = panel.size + "em";
      fixedSize += panel.size;
    }
  }

  for (var i = 0; i < panels.length; ++i) {
    var panel = panels[i];
    createPanel(panel);
    if (opt_spacing && i < panels.length - 1) {
      createPanel({size: opt_spacing});
    }
  }

  flexFrame.style.bottom = fixedSize + "em";
  layoutDiv.appendChild(flexFrame);
  return layoutDiv;
}


function createHorizontalLayout(panels, opt_spacing) {

  // TODO: When the overall size is too small, blocks extend past the right boundary.

  var flexCount = prepareLayout(panels);

  var layoutDiv = createDiv("layout");
  layoutDiv.setAttribute("metaclass", "layout");
  layoutDiv.setAttribute("layoutDirection", "horizontal");

  var flexFrame = createDiv("flex-frame");
  var fixedSize = 0;
  var flexSize = 0;
  var flexBlock = null;

  function createPanel(panel) {

    if (!flexBlock) {
      flexBlock = createDiv("flex-block");
      flexBlock.style.left = (100 * flexSize / flexCount) + "%";
      flexFrame.appendChild(flexBlock);
    }

    var container = createDiv("container");
    flexBlock.appendChild(container);

    var box = createDiv("box");
    box.style.left = fixedSize + "em";
    if (panel.box) {
      box.appendChild(panel.box);
    }
    container.appendChild(box);

    if (panel.flex) {
      flexSize += panel.flex;
      flexBlock.style.width = (100 * panel.flex / flexCount) + "%";
      /* TODO if (panel.minSize) {
        flexBlock.style.minWidth = panel.minSize + "em";
      }*/
      box.style.width = "100%";
      flexBlock = null;
    } else {
      box.style.width = panel.size + "em";
      fixedSize += panel.size;
    }
  }

  for (var i = 0; i < panels.length; ++i) {
    var panel = panels[i];
    createPanel(panel);
    if (opt_spacing && i < panels.length - 1) {
      createPanel({size: opt_spacing});
    }
  }

  flexFrame.style.right = fixedSize + "em";
  layoutDiv.appendChild(flexFrame);
  return layoutDiv;
}


function createVerticalAutoLayout(panels, opt_spacing) {

  var layoutTable = createElementEx("table");
  layoutTable.setAttribute("border", "0");
  layoutTable.setAttribute("cellspacing", "0");
  layoutTable.setAttribute("cellpadding", "0");

  function createPanel(panel) {

    var tr = createElementEx("tr");
    layoutTable.appendChild(tr);

    var td = createElementEx("td");
    td.style.display = "block";
    td.style.padding = "0 0 0 0";
    tr.appendChild(td);

    if (panel.box) {
      td.appendChild(panel.box);
    }

    if (panel.size) {
      if (panel.box) {
        if (!panel.flex) {
          panel.box.style.height = panel.size + "em";
        }
        // TODO: %-based height isn't respected.
      } else {
        var div = createDiv(null, " ");
        div.style.display = "block";
        div.style.height = panel.size + "em";
        td.appendChild(div);
      }
    }
  }

  for (var i = 0; i < panels.length; ++i) {
    var panel = panels[i];
    createPanel(panel);
    if (opt_spacing && i < panels.length - 1) {
      createPanel({size: opt_spacing});
    }
  }

  return layoutTable;
}


function createHorizontalAutoLayout(panels, opt_spacing) {

  var flexCount = 0;
  for (var i = 0; i < panels.length; ++i) {
    var panel = panels[i];
    if (panel.flex) {
      flexCount += panel.flex;
    }
  }

  var layoutTable = createElementEx("table");
  layoutTable.style.width = "100%";
  layoutTable.style.height = "100%";
  layoutTable.setAttribute("border", "0");
  layoutTable.setAttribute("cellspacing", "0");
  layoutTable.setAttribute("cellpadding", "0");

  var tr = createElementEx("tr");
  tr.style.verticalAlign = "top";
  layoutTable.appendChild(tr);

  var fixedSize = 0;
  var flexBlock = null;

  function createPanel(panel) {

    var td = createElementEx("td");
    tr.appendChild(td);

    if (panel.box) {
      td.appendChild(panel.box);
    } else {
      var div = createDiv(null, " ");
      div.style.display = "block";
      div.style.width = panel.size + "em";
      td.appendChild(div);
    }

    if (panel.flex) {
      td.setAttribute("width", (100 * panel.flex / flexCount) + "%");
    } else if (panel.size) {
      td.setAttribute("width", panel.size + "em");
    }

    td.style.whiteSpace = "nowrap";
  }

  for (var i = 0; i < panels.length; ++i) {
    var panel = panels[i];
    createPanel(panel);
    if (opt_spacing && i < panels.length - 1) {
      createPanel({size: opt_spacing});
    }
  }

  return layoutTable;
}


function createHorizontalAutoLayoutSimple(panels, opt_spacing) {

  var div = createDiv();

  for (var i = 0; i < panels.length; ++i) {

    var panel = panels[i];
    var span = createSpan();

    if (opt_spacing && i > 0) {
      span.style.marginLeft = opt_spacing + "em";
    }

    if (panel.box) {
      span.appendChild(panel.box);
    }

    div.appendChild(span);
  }

  return div;
}
