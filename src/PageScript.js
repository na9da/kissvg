"use strict";

exports.setGlobalVariable = function(name) {
    return function(val) {
        return function() {
            if (typeof window != "undefined") {
                window[name] = val
            }
        }
    }
}
