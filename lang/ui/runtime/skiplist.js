// TODO: Using skiplists could be misguided. Since we don't really need to
// maintain a sorted order, using JS associative arrays might turn out to be
// a lot faster...

function SkipList(opt_fnCmp, opt_avgSize) {
  if (!opt_fnCmp) {
    opt_fnCmp = function(a, b) {
      if (a < b) {
        return -1;
      } else if (a == b) {
        return 0;
      } else {
        return 1;
      }
    }
  }
  if (!opt_avgSize) { opt_avgSize = 128; }
  this.fnCmp = opt_fnCmp;
  this.maxHeight = Math.round(Math.log(opt_avgSize) / Math.log(2));
  this.head = new SkipListNode(null, null, this.maxHeight);
}


function SkipListNode(key, value, height) {
  this.key = key;
  this.value = value;
  this.links = new Array(height);
  this.height = height;
}


SkipListNode.prototype.toString = function() {
  return "{ key: " + (this.key ? this.key : "(null)") +
         ", value: " + (this.value ? this.value : "(null)") + " }";
}


SkipList.prototype.iterator = function() {
  var it = new Object();
  it.node = this.head.links[0];
  it.moreData = function() { return it.node; }
  it.get = function() { return { key: it.node.key, value: it.node.value }; }
  it.moveNext = function() {
    if (it.node) {
      it.node = it.node.links[0];
      return true;
    } else {
      return false;
    }
  };
  return it;
}


SkipList.prototype.doList = function(fn) {
  var node = this.head.links[0];
  while (node) {
    fn(node.key, node.value);
    node = node.links[0];
  }
}


SkipList.prototype.get = function(key) {

  var node = this.head;
  var level = this.maxHeight - 1;

  while (node && level >= 0) {

    var next = node.links[level];

    var cmp = next ? this.fnCmp(next.key, key) : 1;
    if (cmp < 0) {
      node = next;
    } else if (cmp == 0) {
      return next.value;
    } else {
      --level;
    }
  }

  return null;
}


SkipList.prototype.insert = function(key, value) {

  var newNode = new SkipListNode(key, value, this.genNodeHeight());

  var node = this.head;
  var level = this.maxHeight - 1;

  while (node && level >= 0) {

    var next = node.links[level];

    if (level < newNode.height) {
      var cmp = next ? this.fnCmp(next.key, key) : 1;
      if (cmp < 0) {
        node = next;
      } else if (cmp == 0) {
        return { found: true, value: next.value };
      } else {
        newNode.links[level] = node;
        --level;
      }
    } else {
      --level;
    }
  }

  doArray(newNode.links,
    function(node, level) {
      newNode.links[level] = node.links[level];
      node.links[level] = newNode;
    });

  return { found: false, value: newNode.value };
}


SkipList.prototype.remove = function(key) {

  var node = this.head;
  var level = this.maxHeight - 1;
  var found = false;
  var value = null;

  while (node && level >= 0) {

    var next = node.links[level];

    var cmp = next ? this.fnCmp(next.key, key) : 1;
    if (cmp < 0) {
      node = next;
    } else if (cmp == 0) {
      node.links[level] = next.links[level];
      found = true;
      value = next.value;
    } else {
      --level;
    }
  }

  return { found: found, value: value };
}


SkipList.prototype.genNodeHeight = function() {
  var rnd = Math.round(Math.random() * 0xffffffff);
  var height = 1;
  while (height < this.maxHeight && rnd % 2 == 0) { rnd /= 2; ++height; }
  return height;
}


// Sample output:
//
//  |-------------------------->|
//  |----->|------------>|----->|
//  |----->|----->|----->|----->|
// (*)    (1)    (2)    (3)    (4)
//

SkipList.prototype.toString = function() {

  var lines = new Array(this.maxHeight + 1);
  var curLine = 0;
  var labelSize = 4;
  var ptrLine = ""; for (var i = 0; i < labelSize; ++i) { ptrLine += "-"; }

  // Association from node keys to indices.
  var indexMap = new Array();

  // Print nodes and remember each node's index.
  var line = "";
  var node = this.head;
  var index = 0;
  while (node) {

    var key = (node == this.head) ? "*" : node.key.toString().substring(0, labelSize);
    indexMap[index++] = node;
    node = node.links[0];

    line += "(";
    line += key;
    line += ")";
    for (var i = key.length + 2; i <= labelSize + 2; ++i) { line += " "; }
  }

  lines[curLine++] = line;

  // Print pointers at each level.
  for (var level = 0; level < this.maxHeight; ++level) {
    line = " |";
    node = this.head;
    index = 0;
    while (node.links[level] != null) {
      node = node.links[level];
      while (indexMap[++index] != node) { line += ptrLine + "---"; }
      line += ptrLine + "->|";
    }

    lines[curLine++] = line;
  }

  // Output to string.
  var str = "";
  for (var i = lines.length - 1; i >= 0; --i) { str += lines[i] + "\n"; }
  return str;
}

