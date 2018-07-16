"use strict";

exports.parseFloatImpl = function(Nothing) {
    return function(Just) {
        return function(s) {
            const val = parseFloat(s)
            if (isNaN(val))
                return Nothing
            return Just(val)
        }
    }
}
