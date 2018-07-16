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

exports.parentElementImpl = function(node) {
    return function() {
        return node.parentElement
    }
}
