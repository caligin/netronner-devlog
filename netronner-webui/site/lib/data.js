/**
 * 
 * Depends on:
 *  - lambda-driver
 *  - jQuery for $.ajax
 */

objects.namespace('Anima.data');

Anima.data.Model = function(/**/) {
    var fields = Array.isArray(arguments[0]) ? arguments[0] : Array.prototype.slice.call(arguments, 0);
    this.fieldCtors = fields.map(this._makeFieldCtor);
};
Anima.data.Model.prototype._makeFieldCtor = function(fieldDef) {
    if (typeof fieldDef === 'string') {
        return function(raw, data) {
            data[fieldDef] = raw[fieldDef];
            return data;
        };
    }
    if (typeof fieldDef === 'function') {
        return fieldDef;
    }
    var name = fieldDef.name;
    var mapping = fieldDef.mapping || name;
    var convert = fieldDef.convert || function(v, raw) {
        return v;
    };
    return function(raw, data) {
        data[name] = convert(objects.deepPluck(mapping, raw), raw);
        return data;
    };
};
Anima.data.Model.prototype.makeRecord = function(raw) {
    return new Anima.data.Record(this, raw);
};

Anima.data.Record = function(model, raw) {
    var data = model.fieldCtors.reduce(function(newData, ctor) {
        return ctor(raw, newData);
    }, {});
    this.data = data;
    this.raw = raw;
};
Anima.data.Record.prototype.get = function(fieldName) {
    //FIXME this dows not allow to get nested stuff.
    return this.data[fieldName];
};
Anima.data.Record.prototype.maybeGet = function(fieldName) {
    //FIXME this dows not allow to get nested stuff.
    return this.data.hasOwnProperty(fieldName) ? [this.data[fieldName]] : [];
};
// url, fields
Anima.data.Store = function(config) {
    dbc.precondition.keysDefined(config, ["url"]);
    this.url = config.url;
    this.model = new Anima.data.Model(config.fields || []);
    this.data = [];
    this._events = {};
};
// callback
Anima.data.Store.prototype.load = function(options) {
    var self = this;
    var callback = (typeof options === 'function' ? options : options && options.callback) || noop;
    $.ajax({
        url: self.url,
        success: function(elements) {
            self.data = elements.map(self.model.makeRecord.bind(self.model));
            callback();
            self.fireEvent('load', self.data, true);
            self.fireEvent('load:success', self.data);
        },
        error: function() {
            callback();
            self.fireEvent('load', [], false);
            self.fireEvent('load:failure');
        }
    });
};
Anima.data.Store.prototype.on = function(event, handler) {
    var hls = objects.provide(this._events, event, {handlers: [], singles: []});
    hls.handlers.push(handler);
};
Anima.data.Store.prototype.once = function(event, handler) {
    var hls = objects.provide(this._events, event, {handlers: [], singles: []});
    hls.singles.push(handler);
};
Anima.data.Store.prototype.fireEvent = function(event /*, arguments*/) {
    var args = Array.prototype.slice.call(arguments, 1);
    var scope = this;
    if (!this._events[event]) {
        return;
    }
    this._events[event].handlers.concat(this._events[event].singles).forEach(function(hl) {
        hl.apply(scope, args);
    });
    this._events[event].singles = [];
};
Anima.data.Store.prototype.findExact = function(field, value) {
    for (var r in this.data) {
        if (r.get(field) === value) {
            return [r];
        }
    }
    return [];
}