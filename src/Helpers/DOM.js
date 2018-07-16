"use strict";

exports.scrollWidthImpl = function(el) {
    return function() {
        return el.scrollWidth
    }
}

exports.scrollHeightImpl = function(el) {
    return function() {
        return el.scrollHeight
    }
}

