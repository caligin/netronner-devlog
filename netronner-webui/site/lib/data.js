/**
 * 
 * Depends on:
 *  - lambda-driver
 *  - jQuery for $.ajax
 */

objects.namespace('Anima.data');

Anima.data.Model = function(/*[fields...] or fields...*/) {
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
    this.url = config.url;
    this.rootProperty = config.rootProperty ? [config.rootProperty] : [];
    this.model = new Anima.data.Model(config.fields || []);
    this.data = config.data || [];
    objects.mixin(this, Anima.Observable);
};
// callback
Anima.data.Store.prototype.load = function(options) {
    var self = this;
    var callback = (typeof options === 'function' ? options : options && options.callback) || noop;
    if(!this.url){
        callback();
        self.fireEvent('load', self.data, null);
        return;
    }
    $.ajax({
        url: self.url,
        success: function(data) {
            var elements = self.rootProperty.isEmpty() ? data : data[self.rootProperty.fst()]
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

Anima.data.Store.prototype.findExact = function(field, value) {
    for (var r in this.data) {
        if (r.get(field) === value) {
            return [r];
        }
    }
    return [];
};

Anima.Observable = function() {
    this._events = {};
};
Anima.Observable.prototype.on = function(event, handler) {
    var hls = objects.provide(this._events, event, {handlers: [], singles: []});
    hls.handlers.push(handler);
};
Anima.Observable.prototype.once = function(event, handler) {
    var hls = objects.provide(this._events, event, {handlers: [], singles: []});
    hls.singles.push(handler);
};
Anima.Observable.prototype.fireEvent = function(event /*, arguments*/) {
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