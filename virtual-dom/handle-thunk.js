var isVNode = require("vtree/is-vnode")
var isVText = require("vtree/is-vtext")
var isWidget = require("vtree/is-widget")
var isThunk = require("vtree/is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    return { a: isThunk(a) ? renderThunk(a, null) : null
           , b: isThunk(b) ? renderThunk(b, a) : null
           }
}

function renderThunk(thunk, previous) {
    if(thunk.vnode) return null;
    thunk.render(previous);
    return thunk.vnode ? null : thunk;
}
