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

exports.innerTextImpl = function(el) {
    return function() {
        return el.innerText
    }
}

exports.yImpl = function(node) {
    return function() {
        var rect
        if (node.getBoundingClientRect) {
            rect = node.getBoundingClientRect()

        } else {
            const range = document.createRange()
            range.selectNodeContents(node)
            rect = range.getBoundingClientRect()
        }
        return rect.y
    }
}

exports.querySelectorImpl = function(Nothing) {
    return function(Just) {
        return function(selector) {
            return function() {
                const el = document.querySelector(selector)
                if (el) {
                    return Just(el)
                } else {
                    return Nothing
                }
            }
        }
    }
}
