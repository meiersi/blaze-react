/*
  Bindings to ReactJS.

  To generate lib.js do:

     $ npm install
     $ grunt

 */


var React     = require('react');
var invariant = require('./invariant');

/**
 * Construct a new React.DOM node.
 *
 * @param {string} tag                 the you'd like to generate
 * @param {object} props               key-value list of object-properties
 * @param {ReactComponent[]} children  the node's children
 *
 */
function mkDomNode(tag, props, children) {
    'use strict';

    var mkTag = React.DOM[tag];
    invariant
      (!mkTag, 'Tried to construct unsupported ReactJS DOM node %s.', tag);
    return mkTag(props, children);
}

var AppComponent = React.createClass({
        displayName: "GhcJSApp",
        render: function() { return this.props.onRender(); }
    });

function mountApp(domNode, renderCb) {
    return { renderCb: renderCb,
             domNode : domNode
           };
}

function syncRedrawApp(app) {
    React.renderComponent(AppComponent({onRender: renderCb}), app.domNode);
}

module.exports =
    { mkDomNode:     mkDomNode,
      mountApp:      mountApp,
      syncRedrawApp: syncRedrawApp
    };

// the global variable we're using in the bindings
h$reactjs = module.exports;

// TODO (meiersi): consider registering individual callbacks with ReactJS
// event handlers. Moreover, we'll need to use extensible retention once we
// want to memoize render-calls by introducing custom components.
// h$registerExtensibleRetention(scanTree);
