"use strict";

exports.getBoundingBoxImpl = function(el) {
    return function() {
        const rect = el.getBoundingClientRect()
        const css = window.getComputedStyle(el)

        var left = rect.left + window.scrollX,
            top = rect.top + window.scrollY,
            width, height;
        
        if (css.getPropertyValue('overflow-x') == 'visible') {
            if (el.scrollWidth > rect.width) {
                width = el.scrollWidth
            }
        } 

        if (css.getPropertyValue('overflow-y') == 'visible') {
            if (el.scrollHeight > rect.height) {
                height = el.scrollHeight
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
