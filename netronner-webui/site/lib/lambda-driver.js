String.prototype.format = function() {
    var formatArgs = Array.prototype.slice.call(arguments);
    return formatArgs.reduce(function(memo, arg, i) {
        return memo.replace(new RegExp("\\{" + i + "\\}", "g"), arg);
    }, this.valueOf());
};

objects = {};

// merges n objects in given order (leftmost on bottom, rightmost on top)
objects.merge = function() {
    return Array.prototype.slice.call(arguments).reduce(function(merged, mergee) {
        for (var k in mergee) {
            merged[k] = mergee[k];
        }
        return merged;
    }, {});
};

objects.global = (function() {
    var glob = this;
    return function() {
        return glob;
    };
})();

objects.namespace = function(namespace) {
    return namespace.split('.').reduce(function(memo, piece) {
        memo[piece] = {};
        return memo[piece];
    }, this.global());
};

objects.shallowCopy = function(original) {
    var copy = {};
    for (var k in original) {
        copy[k] = original[k];
    }
    return copy;
};

objects.remove = function(obj /*, properties... */) {
    var removed = {};
    var toRemove = Array.prototype.slice.call(arguments, 1);
    toRemove.forEach(function(prop) {
        if (obj.hasOwnProperty(prop)) {
            removed[prop] = obj[prop];
            delete obj[prop];
        }
    });
    return removed;
};

objects.provide = function(obj, property, initializer) {
    if (!obj.hasOwnProperty(property)) {
        obj[property] = typeof initializer === 'Function' ? initializer(obj, property) : initializer;
    }
    return obj[property];
};

objects.pluck = function(prop, obj) {
    return obj[prop];
};

objects.deepPluck = function(nestedProps, obj) {
    var props = nestedProps.split('.');
    return props.reduce(function(o, nested) {
        return o[nested];
    }, obj);
};

objects.global().noop = function() {
};

objects.namespace('dbc.precondition');

dbc.precondition.defined = function(e, message) {
    if (e === undefined) {
        throw message;
    }
};
dbc.precondition.keysDefined = function(obj /*[keys...] or keys...*/) {
    var keys = Array.isArray(arguments[1]) ? arguments[1] : Array.prototype.slice.call(arguments, 1);
    keys.forEach(function(k) {
        if (obj[k] === undefined) {
            throw "{0} must be defined".format(k);
        }
    });
};
