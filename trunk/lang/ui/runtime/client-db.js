/*

This module implements a simple client-side in-memory relational database.

The following features are supported:
- First-class types with introspection.
- Many views are updatable.
- Change notifications at relation and value levels.
- Derived values.

Not supported:
- Permanent storage (in plans using the browser cache).
- Transactions (planned support for single transaction with rollback).

*/


//
// Event sink type.
//
// Represents a notification sink for events coming from a database value.
//
// Many events are handled by two functions: one that executes before the event,
// and one that executes after. Neither is required. By throwing an exception,
// a "before" function can block an event. Because a subsequent "before"
// function can do the same, no such function should have material side
// effects--they belong in "after" functions, which can assume the event has
// occurred.
//

function EventSink(value) {
  insertListHead(value.eventSinkListHead, this, "eventSinkListEntry");
  this.remove = function() { removeListEntry(this.eventSinkListEntry); }
}


//
// Value interface.
//

function Value_init(value, type) {
  value.type = type;
  initListHead(value, "eventSinkListHead");
  value.dispatchEvent = Value_dispatchEvent;
}


function Value_dispatchEvent(eventHandlerName, args) {

  for (var le = this.eventSinkListHead.flink;
       le != this.eventSinkListHead;
       le = le.flink) {

    var es = le.obj;
    var fn = es[eventHandlerName];
    if (fn) { fn.apply(null, args); }
  }
}


//
// SimpleValue interface.
//

function SimpleValue_get() { return this.data; }


function SimpleValue_set(x) {
  this.type.check(x);
  this.dispatchEvent("beforeUpdate", [x]);
  var oldX = this.data;
  this.data = x;
  this.dispatchEvent("afterUpdate", [oldX, x]);
}


//
// String type.
//

function StringType() {
  this.minLen = null;
  this.maxLen = null;
}


StringType.prototype.createValue = function(opt_init) {
  return new StringValue(this, opt_init);
}


StringType.prototype.check = function(x) {
  var len = x.length;
  if (this.minLen && len < this.minLen) {
    throw "'" + x + "' is too short: minLen is " + this.minLen + ".";
  }
  if (this.maxLen && this.maxLen < len) {
    throw "'" + x + "' is too long: maxLen is " + this.maxLen + ".";
  }
}


function StringValue(type, opt_init) {
  Value_init(this, type);
  if (opt_init) { this.data = opt_init; }
}


StringValue.prototype.get = SimpleValue_get;
StringValue.prototype.set = SimpleValue_set;

StringValue.prototype.toString = function() {
  return '"' + this.data + '"';
}


//
// Integer type.
//

function IntegerType() {
  this.min = null;
  this.max = null;
}


IntegerType.prototype.createValue = function(opt_init) {
  return new IntegerValue(this, opt_init);
}


IntegerType.prototype.check = function(x) {
  if (this.min && x < this.min) {
    throw "'" + x + "' is too small: min is " + this.min + ".";
  }
  if (this.max && this.max < x) {
    throw "'" + x + "' is too large: max is " + this.max + ".";
  }
}


function IntegerValue(type, opt_init) {
  Value_init(this, type);
  if (opt_init != undefined) { this.data = opt_init; }
}


IntegerValue.prototype.get = SimpleValue_get;
IntegerValue.prototype.set = SimpleValue_set;

IntegerValue.prototype.toString = function() {
  return '' + this.data;
}


//
// Boolean type.
//

function BooleanType() {}


BooleanType.prototype.createValue = function(opt_init) {
  return new BooleanValue(this, opt_init);
}


BooleanType.prototype.check = function(x) {}


function BooleanValue(type, opt_init) {
  Value_init(this, type);
  if (opt_init != undefined) { this.data = opt_init; }
}


BooleanValue.prototype.get = SimpleValue_get;
BooleanValue.prototype.set = SimpleValue_set;

BooleanValue.prototype.toString = function() {
  return '' + this.data;
}


//
// Object type.
//

function ObjectType() {}


ObjectType.prototype.createValue = function(opt_init) {
  return new ObjectValue(this, opt_init);
};


ObjectType.prototype.check = function(x) {};


function ObjectValue(type, opt_init) {
  Value_init(this, type);
  if (opt_init) { this.data = opt_init; } else { this.data = undefined; }
}


ObjectValue.prototype.get = SimpleValue_get;
ObjectValue.prototype.set = SimpleValue_set;

ObjectValue.prototype.toString = function() {
  return '' + this.data;
};


//
// Tuple type.
//
// Parameters are objects: { name: ..., type: ...};
// one parameter per tuple attribute.
//
// A tuple's "value" property is an array of attribute values.
//
// Each tuple attribute can be retrieved through a function named the same
// as the attribute name, e.g. { name: id, type: ... } is retrieved by tuple.id().
//

function TupleType( /* ... */ ) {

  this.attributeDescriptors = arguments;

  // Assign indices.
  doArray(this.attributeDescriptors, function(ad, i) { ad.index = i; });
}


TupleType.prototype.createValue = function(opt_init) {
  return new TupleValue(this, opt_init);
}


TupleType.prototype.check = function(x) {
  doArray(this.attributeDescriptors,
    function(ad, i) { ad.type.check(x[i]); });
}


function TupleValue(type, opt_init) {

  Value_init(this, type);

  this.data = new Array();
  var value = this;

  doArray(type.attributeDescriptors,
    function(ad, i) {

      // Initial value.
      value.data[i] = ad.type.createValue(opt_init ? opt_init[i] : null);

      // Attribute accessors.
      value[ad.name] = function() { return value.data[i]; }

      // Update event sinks.
      var updateEventSink = new EventSink(value.data[i]);
      updateEventSink.beforeUpdate = function(x) {
        value.dispatchEvent("beforeAttributeUpdate", [i, x]);
      }
      updateEventSink.afterUpdate = function(oldX, x) {
        value.dispatchEvent("afterAttributeUpdate", [i, oldX, x]);
      }
    });
}


TupleValue.prototype.toString = function() {
  var value = this;
  var s = "{";
  doArray(this.type.attributeDescriptors,
    function(ad, i) {
      if (i > 0) { s += ", "; }
      s += ad.name + ": " + value.data[i];
    });
  s += "}";
  return s;
}


//
// Relation type.
//
// Parameters:
// generatorType - a tuple type.
// primaryKey - name of the primary key attribute within the tuple type.
// secondaryIndices - array of names of attributes to use for secondary indices.
//
// The primary key cannot be changed once a tuple is inserted into a relation.
// Same applies to keys used for secondary indices.
//
// Secondary indices are not unique.
//

function RelationType(generatorType, primaryKey, opt_secondaryIndices) {
  this.generatorType = generatorType;
  this.primaryKeyName = primaryKey;
  if (opt_secondaryIndices) { this.secondaryIndices = opt_secondaryIndices; }
}


RelationType.prototype.createValue = function() {
  return new BaseRelationValue(this);
}


function getPrimaryKeyValue(reltype, tuple) {
  // This works, because PK values cannot change.
  if (!tuple.momoizedPkValue) {
    tuple.momoizedPkValue = tuple[reltype.primaryKeyName]().get()
  }
  return tuple.momoizedPkValue;
}


function getSecondaryKeyValue(reltype, indexName, tuple) {
  return tuple[indexName]().get();
}


function keyCmp(pkA, pkB) {
  if (pkA < pkB) {
    return -1;
  } else if (pkA == pkB) {
    return 0;
  } else {
    return 1;
  }
}


function BaseRelationValue(type) {

  Value_init(this, type);
  this.data = new SkipList(keyCmp);

  if (type.secondaryIndices) {
    var secondaryIndexMap = new Object();
    this.secondaryIndexMap = secondaryIndexMap;
    this.secondaryIndices = mapArray(type.secondaryIndices,
      function(si) {
        var indexDescriptor = { name: si, index: new SkipList(keyCmp) };
        secondaryIndexMap[si] = indexDescriptor;
        return indexDescriptor;
      });
  }
}


BaseRelationValue.prototype.iterator = function() {
  var it = this.data.iterator();
  it.oldGet = it.get;
  it.get = function() { return it.oldGet().value; }
  return it;
}


BaseRelationValue.prototype.get = function(key) {
  return this.data.get(key);
}


BaseRelationValue.prototype.getSecondary = function(index, key) {
  var matchingPks = this.secondaryIndexMap[index].index.get(key);
  var This = this;
  return mapArray(matchingPks, function(pk) { return This.get(pk); }, true);
}


BaseRelationValue.prototype.insert = function(tuple) {

  this.dispatchEvent("beforeInsert", [tuple]);

  var found = this.data.insert(getPrimaryKeyValue(this.type, tuple), tuple).found;
  if (found) { throw "Duplicate key."; }

  var This = this;
  doArray(this.secondaryIndices, function(si) { This.indexValue(si, tuple); });
  this.installChangeNotificationHandlers(tuple);

  this.dispatchEvent("afterInsert", [tuple]);
}


BaseRelationValue.prototype.remove = function(key) {

  this.dispatchEvent("beforeRemove", [key]);

  var removeResult = this.data.remove(key);

  if (removeResult.found && this.secondaryIndices) {

    this.uninstallChangeNotificationHandlers(removeResult.value);
    var This = this;
    doArray(this.secondaryIndices,
      function(si) { This.unindexValue(si, removeResult.value); });
  }

  if (removeResult.found) {
    this.dispatchEvent("afterRemove", [key]);
  }

  return removeResult.found;
}



BaseRelationValue.prototype.select = function(pred) {
  return new SelectRelationValue(this, pred);
}


BaseRelationValue.prototype.project = function(attributeDescriptors,
                                               opt_mappingFunction,
                                               opt_inverseKeyFunction,
                                               opt_inverseTupleFunction) {
  return new ProjectRelationValue(this,
                                  attributeDescriptors,
                                  opt_mappingFunction,
                                  opt_inverseKeyFunction,
                                  opt_inverseTupleFunction);
}


BaseRelationValue.prototype.join = function(thisPrefix, otherPrefix, other, foreignKey) {
  return new JoinRelationValue(thisPrefix, this, otherPrefix, other, foreignKey);
}


BaseRelationValue.prototype.installChangeNotificationHandlers = function(tuple) {
  tuple.secondaryIndexEventSinks = new Array();
  var This = this;
  doArray(this.secondaryIndices,
    function(si) {
      var handler = new EventSink(tuple[si.name]());
      handler.beforeUpdate = function(x) { This.unindexValue(si, tuple); }
      handler.afterUpdate = function(oldX, x) { This.indexValue(si, tuple); };
      appendArray(tuple.secondaryIndexEventSinks, handler);
    });
}


BaseRelationValue.prototype.uninstallChangeNotificationHandlers = function(tuple) {
  doArray(tuple.secondaryIndexEventSinks, function(es) { es.remove(); });
  tuple.secondaryIndexEventSinks = null;
}


BaseRelationValue.prototype.indexValue = function(secondaryIndex, tuple) {
  var sk = getSecondaryKeyValue(this.type, secondaryIndex.name, tuple);
  if (sk) {
    var pkArray = new Array();
    var value = secondaryIndex.index.insert(sk, pkArray).value;
    appendArray(value, getPrimaryKeyValue(this.type, tuple));
  }
}


BaseRelationValue.prototype.unindexValue = function(secondaryIndex, tuple) {
  var sk = getSecondaryKeyValue(this.type, secondaryIndex.name, tuple);
  if (sk) {
    var pkArray = secondaryIndex.index.get(sk);
    removeFromArray(pkArray, getPrimaryKeyValue(this.type, tuple));
  }
}


BaseRelationValue.prototype.toString = function() {

  var s = "";
  var colWidth = 24;

  var hr = repeatString("+" + repeatString("-", colWidth + 2),
                        this.type.generatorType.attributeDescriptors.length) +
           "+\n";

  // Header.
  s += hr;
  s += "|";
  doArray(this.type.generatorType.attributeDescriptors,
    function(ad, i) {
      s += " " + padString(ellipseString(ad.name, colWidth), colWidth) + " |";
    });
  s += "\n";
  s += hr;

  // Values
  for (var it = this.iterator(); it.moreData(); it.moveNext()) {
    var tuple = it.get();
    s += "|";
    doArray(this.type.generatorType.attributeDescriptors,
      function(ad, i) {
        s += " " + padString(ellipseString("" + tuple.data[i], colWidth),
                             colWidth) + " |";
      });
    s += "\n";
  }

  s += hr;
  return s;
}


//
// Relation derived by selection.
//
// TODO:
// - Create predicate types that can be combined using (, ), AND, OR, NOT.
// - For now only support PK= and general JS function predicates.
// - Add support for secondary indices with = operation.
// - Set up change notification handlers on base relation value and trigger
//   insert/delete change notifications in the selection. Same for projections.
//

function SelectRelationValue(base, pred) {
  Value_init(this, base.type);
  this.base = base;
  this.pred = pred;
}


SelectRelationValue.prototype.iterator = function() {

  var pred = this.pred;
  var it = this.base.iterator();
  var wrapper = new Object();

  wrapper.get = function() { return wrapper.nextDatum; }
  wrapper.moreData = wrapper.get;

  wrapper.moveNext = function() {
    wrapper.nextDatum = null;
    while (it.moreData()) {
      var datum = it.get();
      it.moveNext();
      if (pred(datum)) { wrapper.nextDatum = datum; break; }
    }
  };

  wrapper.moveNext();
  return wrapper;
}


SelectRelationValue.prototype.get = function(key) {
  var tuple = this.base.get(key);
  if (this.pred(tuple)) {
    return tuple;
  } else {
    return null;
  }
}


SelectRelationValue.prototype.insert = function(tuple) {

  this.dispatchEvent("beforeInsert", [tuple]);

  if (!this.pred(tuple)) {
    throw "Tuple " + tuple + " does not match the selection predicate.";
  }

  this.base.insert(tuple);

  this.dispatchEvent("afterInsert", [tuple]);
}


SelectRelationValue.prototype.remove = function(key) {

  this.dispatchEvent("beforeRemove", [key]);

  var tuple = this.base.get(key);
  if (!tuple) { return false; }
  if (!this.pred(tuple)) {
    throw "Deleted tuple " + tuple + " does not match the selection predicate.";
  }

  this.base.remove(key);

  this.dispatchEvent("afterRemove", [key]);
  return true;
}


SelectRelationValue.prototype.select = BaseRelationValue.prototype.select;
SelectRelationValue.prototype.project = BaseRelationValue.prototype.project;
SelectRelationValue.prototype.join = BaseRelationValue.prototype.joid;
SelectRelationValue.prototype.toString = BaseRelationValue.prototype.toString;


//
// Relation derived by projection.
//
// A projected relation is defined by a set of attributes and forward/inverse
// mapping functions (between original tuples and mapped tuples).
//
// Each attribute descriptor is of the form:
// { name: ..., type: ..., primaryKey: true | false }
//
// If no primary key is identified, the projection is read-only.
//
// If no mapping functions are provided, the attributes in the attribute
// descriptor list are identity-mapped.
//
// If opt_mappingFunction is provided and opt_inverse*Function are not provided,
// the projection is read-only.
//

function ProjectRelationValue(base, attributeDescriptors, opt_mappingFunction,
                              opt_inverseKeyFunction, opt_inverseTupleFunction) {

  // Prepare new relation type.
  var primaryKey = null;

  doArray(attributeDescriptors, function(ad, i) {
    ad.index = i;
    if (ad["primaryKey"]) {
      if (primaryKey) { throw "Multiple primary keys."; }
      primaryKey = ad.name;
    }
  });

  var generatorType = new TupleType();
  generatorType.attributeDescriptors = attributeDescriptors;

  var relType = new RelationType(generatorType, primaryKey);

  if (opt_mappingFunction) {

    relType.map = function(tuple) {

      var mt = opt_mappingFunction(tuple);

      var mappedTuple = generatorType.createValue(
        mapArray(attributeDescriptors, function(ad) { return mt[ad.name]; }));

      // If the inverse exists, make the tuple updatable.
      if (opt_inverseKeyFunction && opt_inverseTupleFunction) {
        doArray(mappedTuple.data,
          function(val, i) {
            val.baseSet = val.set;
            val.set = function(x) {

              var newMappedTuple = new Object;
              copyObj(newMappedTuple, mappedTuple);
              newMappedTuple.data[i].baseSet(x);

              var inverseTuple = opt_inverseTupleFunction(newMappedTuple);
              var propName = base.type.generatorType.attributeDescriptors[i].name;
              tuple[propName]().set(inverseTuple[propName]);
            };
          });
      }

      return mappedTuple;
    }

    if (opt_inverseKeyFunction) {

      relType.inverseKeyMap = opt_inverseKeyFunction;

      relType.inverseTupleMap = function(tuple) {
        var it = opt_inverseTupleFunction(tuple);
        return base.type.generatorType.createValue(
          mapArray(base.type.generatorType.attributeDescriptors,
            function(ad) { return it[ad.name]; }));
      }
    }

  } else {

    // Create a mapping routine if one isn't specified.
    relType.map = function(tuple) {
      return generatorType.createValue(
        mapArray(attributeDescriptors,
          function(ad) { return tuple[ad.name]().get(); }));
    }

    if (primaryKey) {

      relType.inverseKeyMap = identity;

      relType.inverseTupleMap = function(tuple) {
        return base.type.generatorType.createValue(
          mapArray(base.type.generatorType.attributeDescriptors,
            function(ad) {
              if (tuple[ad.name]) {
                return tuple[ad.name]().get();
              } else {
                return null;
              }
            }));
        };
    }
  }

  // Initialize value.
  Value_init(this, relType);
  this.base = base;
}


ProjectRelationValue.prototype.iterator = function() {
  var map = this.type.map;
  var it = this.base.iterator();
  var wrapper = new Object();
  wrapper.moreData = it.moreData;
  wrapper.get = function() { return map(it.get()); }
  wrapper.moveNext = it.moveNext;
  return wrapper;
}


ProjectRelationValue.prototype.get = function(key) {
  if (!this.type.inverseKeyMap) { throw "Inverse mapping required."; }
  var mappedTuple = this.type.map(this.base.get(this.type.inverseKeyMap(key)));
  return mappedTuple;
}


ProjectRelationValue.prototype.insert = function(tuple) {

  if (!this.type.inverseTupleMap) { throw "Inverse mapping required."; }

  this.dispatchEvent("beforeInsert", [tuple]);

  this.base.insert(this.type.inverseTupleMap(tuple));

  this.dispatchEvent("afterInsert", [tuple]);
}


ProjectRelationValue.prototype.remove = function(key) {

  if (!this.type.inverseKeyMap) { throw "Inverse mapping required."; }

  this.dispatchEvent("beforeRemove", [key]);

  return this.base.remove(this.type.inverseKeyMap(key));

  this.dispatchEvent("afterRemove", [key]);
  return true;
}


ProjectRelationValue.prototype.select = BaseRelationValue.prototype.select;
ProjectRelationValue.prototype.project = BaseRelationValue.prototype.project;
ProjectRelationValue.prototype.join = BaseRelationValue.prototype.joid;
ProjectRelationValue.prototype.toString = BaseRelationValue.prototype.toString;


//
// Commonly used form of a join, equivalent to:
//
// SELECT *
// FROM left INNER JOIN right ON left.foreign_key = right.primary_key
//
// Note that returned tuples consist of two values: left tuple, right tuple.
//

function JoinRelationValue(leftPrefix, left, rightPrefix, right, leftForeignKey) {

  // Prepare new relation type.
  var attributeDescriptors = new Array();

  var addAttributeDescriptor = function(prefix) {
    return function(ad) {
      var joinedAd = new Object();
      joinedAd.name = prefix + ad.name;
      joinedAd.type = ad.type;
      joinedAd.index = attributeDescriptors.length;
      appendArray(attributeDescriptors, joinedAd);
    };
  };

  doArray(left.type.generatorType.attributeDescriptors, addAttributeDescriptor(leftPrefix));
  doArray(right.type.generatorType.attributeDescriptors, addAttributeDescriptor(rightPrefix));

  var generatorType = new TupleType();
  generatorType.attributeDescriptors = attributeDescriptors;
  var relType = new RelationType(generatorType, null);

  // TODO: question: if either input is a selection or projection,
  //       how do we reach into the base relation's secondary indices?
  //       answer: these indices can probably be inherited.

  Value_init(this, relType);
  this.leftPrefix = leftPrefix;
  this.left = left;
  this.rightPrefix = rightPrefix;
  this.right = right;
  this.leftForeignKey = leftForeignKey;
}


JoinRelationValue.prototype.iterator = function() {

  var This = this;
  var it = this.left.iterator();
  var wrapper = new Object;
  var tupleType = this.type.generatorType;

  wrapper.get = function() { return wrapper.nextDatum; }
  wrapper.moreData = wrapper.get;

  wrapper.moveNext = function() {

    wrapper.nextDatum = null;
    while (it.moreData()) {

      var leftTuple = it.get();
      it.moveNext();

      var fk = leftTuple[This.leftForeignKey]().get();
      if (!fk) { continue; }  // Allow for nulls.
      var rightTuple = This.right.get(fk);
      if (!rightTuple) { continue; }

      var values = new Array();
      var appendValue = function(v) { appendArray(values, v.get()); }
      doArray(leftTuple.data, appendValue);
      doArray(rightTuple.data, appendValue);

      wrapper.nextDatum = tupleType.createValue(values);
      break;
    }
  };

  wrapper.moveNext();
  return wrapper;
}


JoinRelationValue.prototype.get = null;
JoinRelationValue.prototype.insert = null;
JoinRelationValue.prototype.remove = null;
JoinRelationValue.prototype.select = BaseRelationValue.prototype.select;
JoinRelationValue.prototype.project = BaseRelationValue.prototype.project;
JoinRelationValue.prototype.join = BaseRelationValue.prototype.joid;
JoinRelationValue.prototype.toString = BaseRelationValue.prototype.toString;


/*

T O  D O
--------

JOIN
----
- PK/FK only.

OTHER
-----
 - Union, intersection, difference.

MATERIALIZED RELATIONS
----------------------
- Use derived values. Probably not necessary, unless...:
  Can derived values be used in the DOM? If so, we can create the UI as a DB
  query. We can probably accomplish the same goals using change notifications,
  so it might be an overkill to implement this.

*/


//
// Constrained type generator.
//
// Sample usage (same effect as StringType with minLen):
//
//   return constrainType(new StringType(),
//     function(v) {
//       if (v.get().length < minLen) {
//         throw "'" + v + "' is too short: minLen is " + minLen + ".";
//       }
//     });

function constrainType(baseType, constraintCheck) {

  var type = new Object();
  copyObj(type, baseType);
  type.prototype = baseType.prototype;

  type.check = function(v) {
    baseType.check(v);
    constraintCheck(v);
  }

  return type;
}


//
// ...
//
/*
function DerivedValue(type, deps, fn) {
  // ...
  this.get = function() { return fn.apply (...); }
}
*/
