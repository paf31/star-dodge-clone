"use strict";

// module Main

exports.onSpaceBar = function(f) {
    
    return function() {
        
        window.addEventListener("keydown", function(e) {
            if (e.keyCode === 32) {
                f(true)();
            }
        }, false);
        window.addEventListener("keyup", function(e) {
            if (e.keyCode === 32) {
                f(false)();
            }
        }, false);
    };
};