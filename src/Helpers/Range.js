"use strict";

exports.createImpl = function(node) {
    return function() {
        const range = document.createRange()
        range.selectNodeContents(node)
        range.setStart(node, 0)
        return range
    }
}

exports.setStartImpl = function(node) {
    return function(offset) {
        return function(range) {
            return function() {
                range.setStart(node, offset)
            }
        }
    }
}

exports.startContainerImpl = function(range) {
    return function() {
        return range.startContainer
    }
}

exports.startOffsetImpl = function(range) {
    return function() {
        return range.startOffset
    }
}

exports.collapsedImpl = function(range) {
    return function() {
        return range.collapsed
    }
}

exports.getLineBoundingBoxesImpl = function(range) {
    return function() {
        const rects = Array.prototype.slice.call(range.getClientRects())
        return rects.map(function(r) {
            return {
                left: r.left + window.scrollX,
                right: r.right + window.scrollX,
                top: r.top + window.scrollY,
                bottom: r.bottom + window.scrollY                
            }
        }) 
    }
}


exports.toStringImpl = function(range) {
    return function() {
        return range.toString()
    }
}


