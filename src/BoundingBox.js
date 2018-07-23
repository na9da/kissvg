"use strict";

exports.getBoundingBoxImpl = function(node) {
    return function() {

        var rect, scrollWidth, scrollHeight, css
        rect = node.getBoundingClientRect()
        scrollWidth = node.scrollWidth
        scrollHeight = node.scrollHeight
        css = window.getComputedStyle(node)
            
        var left = rect.left + window.scrollX,
            top = rect.top + window.scrollY,
            width, height;
        
        if (css.getPropertyValue('overflow-x') == 'visible') {
            if (scrollWidth > rect.width) {
                width = scrollWidth
            }
        } 

        if (css.getPropertyValue('overflow-y') == 'visible') {
            if (scrollHeight > rect.height) {
                height = scrollHeight
            }
        }

        const bbox =  {
            left: left,
            top: top,
            width: width || rect.width,
            height: height || rect.height
        }

        return bbox
    }
}
