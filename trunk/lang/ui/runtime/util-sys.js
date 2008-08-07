//
// Simple utility functions.
//

function identity(x) { return x; }


function maybe(x, fn) { if (x) { return fn(x); } else { return null; } }


function doArray(arr, fn) {
  if (arr) {
    for (var i = 0; i < arr.length; ++i) { fn(arr[i], i); }
  }
}


function mapArray(arr, fn, opt_skipNull) {
  var ret = new Array();
  var offset = -1;
  doArray(arr,
    function(val, i) {
      var mapped = fn(val);
      if (mapped || !opt_skipNull) {
        ret[++offset] = mapped;
      }
    });
  return ret;
}


function reduceArray(arr, start, fn) {
  var value = start;
  if (arr) {
    for (var i = 0; i < arr.length; ++i) { value = fn(value, arr[i]); }
  }
  return value;
}


function appendArray(arr, val) {
  arr[arr.length] = val;
}


function findInArray(arr, val, opt_cmp) {
  return reduceArray(arr, false, function(found, item) {
    if (found) { return found; }
    if (opt_cmp) {
      return opt_cmp.call(null, item, val);
    } else {
      return item == val;
    }
  });
}


function removeFromArray(arr, val, opt_cmp) {

  if (arr) {

    if (opt_cmp) {

      for (var i = 0; i < arr.length; ++i) {
        if (opt_cmp.call(null, arr[i], val)) {
          for (var j = i; j < arr.length - 1; ++j) { arr[j] = arr[j + 1]; }
          delete arr[arr.length - 1];
          return true;
        }
      }

    } else {

      for (var i = 0; i < arr.length; ++i) {
        if (arr[i] == val) {
          for (var j = i; j < arr.length - 1; ++j) { arr[j] = arr[j + 1]; }
          delete arr[arr.length - 1];
          return true;
        }
      }
    }
  }

  return false;
}


function sortObjectArray(arr, fnGetKey) {
  arr.sort(function(a, b) {
    var ka = fnGetKey(a);
    var kb = fnGetKey(b);
    if (ka < kb) {
      return -1;
    } else if (kb > kb) {
      return 1;
    } else {
      return 0;
    }
  });
}


function repeatString(s, num) {
  var ret = "";
  while (num-- > 0) { ret += s; }
  return ret;
}


function ellipseString(s, maxLen) {
  if (s.length > maxLen) {
    return s.substring(0, maxLen - 3) + "...";
  } else {
    return s;
  }
}


function padString(s, len) {
  if (s.length < len) {
    return s + repeatString(" ", len - s.length);
  } else {
    return s;
  }
}


function copyObj(target, source) {
  for (var p in source) {
    target[p] = source[p];
  }
}


function initListHead(obj, prop) {
  obj[prop] = { flink: null, blink: null, obj: obj };
  obj[prop].flink = obj[prop].blink = obj[prop];
}


function insertListHead(list, obj, listProp) {
  obj[listProp] = { flink: list.flink, blink: list, obj: obj };
  list.flink.blink = list.flink = obj[listProp];
}


function removeListEntry(listEntry) {
  listEntry.flink.blink = listEntry.blink;
  listEntry.blink.flink = listEntry.flink;
  delete listEntry.flink;
  delete listEntry.blink;
}

