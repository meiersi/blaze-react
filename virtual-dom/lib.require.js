/*
  to generate lib.js, install virtual-dom and process file:

     $ npm install
     $ grunt
   the ./diff module is vtree/diff with a few changes to
   allow diff to run in an asynchronous thread in the presence of
   memoized nodes.
 */

var isVirtualNode = require('vtree/is-vnode');
var isThunk       = require('vtree/is-thunk');
var isArray       = require('x-is-array');
var VPatch        = require("vtree/vpatch")

/** @constructor */
function HSThunk(t, ids, key) {
    this.hst        = t;   // haskell thunk
    this.ids        = ids; // array of haskell unique ids
    this.key        = key;
    this.vnode      = null;
    this._ghcjsMark = 0;
}

HSThunk.prototype.type = 'Thunk';

// render returns the deferred rendering object
// null if the thunk has already been rendered, in which case the value is in this.vnode
HSThunk.prototype.render = function(previous) {
    if(previous && !this.vnode && eqIds(this.ids, previous.ids)) {
        if(previous.hst) {
            this.hst = previous.hst;
        } else {
            this.hst   = null;
            this.vnode = previous.vnode;
        }
    }
    return this.vnode ? null : this;
}

function eqIds(ids1, ids2) {
    if(ids1.length != ids2.length) return false;
    for(var i=ids1.length-1;i>=0;i--) {
        if(ids1[i] !== ids2[i]) return false;
    }
    return true;
}

function forcePatch(p) {
    var thunks = [], i, j, pi;
    for(i in p) {
        var pi = p[i];
        if(isArray(pi))
            for(j=pi.length-1;j>=0;j--) forceTree(pi[j].patch, thunks);
        else if(pi.patch) forceTree(pi.patch, thunks);
        else forceTree(pi, thunks);
    }
    return thunks;
}

function forceTree(n, t) {
    if(isThunk(n)) {
        if(n.vnode) forceTree(n.vnode, t);
        else t.push(n);
    } else if(isVirtualNode(n)) {
        for(var i=n.children.length-1;i>=0;i--) {
            forceTree(n.children[i], t);
        }
    }
}

// add support for more node types here when the bindings can construct them
function scanTree(o, currentMark) {
    if((isVirtualNode(o) || isThunk(o))) {
        if(o._ghcjsMark === currentMark) return true;
        var res = [];
        scanTreeRec(o, res, currentMark);
        return res.length ? res : true;
    } else return false;
}

function scanTreeRec(o, r, currentMark) {
    if(o instanceof HSThunk) {
        if(o._ghcjsMark !== currentMark) {
            o._ghcjsMark = currentMark;
            if(o.t) r.push(o.t);
            else scanTreeRec(o.vnode, r, currentMark);
        }
    } else if(isVirtualNode(o)) {
        if(o._ghcjsMark !== currentMark) {
            o._ghcjsMark = currentMark;
            for(var i=o.children.length-1;i>=0;i--) {
                scanTreeRec(o.children[i], r, currentMark);
            }
        }
    }
}

function setThunkPatch(n, p) {
    if(hasPatches(p)) n.p[n.i] = new VPatch(VPatch.THUNK, null, p);
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true;
        }
    }
    return false;
}

module.exports = { diff:          require('./diff')
                 , HSThunk:       HSThunk
                 , setThunkPatch: setThunkPatch
                 , forceTree:     forceTree
                 , forcePatch:    forcePatch
                 , VNode:         require('vtree/vnode')
                 , VText:         require('vtree/vtext')
                 , patch:         require('vdom/patch')
                 , createElement: require('vdom/create-element')
                 };

// the global variable we're using in the bindings
h$vdom = module.exports;
h$registerExtensibleRetention(scanTree);
