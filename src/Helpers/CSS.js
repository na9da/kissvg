"use strict";

exports.getComputedStyleImpl = function(element) {
    return function() {
        return window.getComputedStyle(element)
    }
}

exports.getPropertyValueImpl = function(prop) {
    return function(css) {
        return function() {
            return css.getPropertyValue(prop)
        }
    }
}
