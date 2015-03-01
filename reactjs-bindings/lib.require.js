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
 * @param {string} tag                 the tag you'd like to generate
 * @param {object} props               key-value list of object-properties
 * @param {ReactComponent[]} children  the node's children
 *
 * DANGER: children will be changed and MUST no be used anymore later.
 *
 */
function mkDomNode(tag, props, children) {
    var mkTag = React.DOM[tag];
    invariant(mkTag, 'Tried to construct unsupported ReactJS DOM node %s.', tag);
    children.unshift(props);
    return mkTag.apply(this, children);
}

var GhcjsApp = React.createClass({
        displayName: "GhcjsApp",
        render:      function() {
                         // FIXME (SM): Undo this resetting of this or report
                         // this bug to GHCJS.
                         //
                         // Find a better way for a callback to return a
                         // value.
                         //
                         var smuggler = {};
                         this.props.onRender.apply(window, [smuggler]);
                         return smuggler.node;
                     }
    });

var GhcjsAppFactory = React.createFactory(GhcjsApp);

function mountApp(domNode, renderCb) {
    return { onRender: renderCb,
             domNode : domNode
           };
}

function syncRedrawApp(app) {
    React.render(GhcjsAppFactory({onRender: app.onRender}), app.domNode);
}

function attachRouteWatcher(routeChangeCb) {
    window.onhashchange = function() {
        routeChangeCb(location.hash);
    };
}

function setRoute(route) {
    // TODO (asayers): Setting location.hash is kinda slow, so ideally we'd
    // wait for a ReactJS animation frame before updating it.
    location.hash = route;
}

module.exports =
    { mkDomNode:          mkDomNode
    , mountApp:           mountApp
    , syncRedrawApp:      syncRedrawApp
    , attachRouteWatcher: attachRouteWatcher
    , setRoute:           setRoute
    };

// the global variable we're using in the bindings
h$reactjs = module.exports;

// TODO (meiersi): consider registering individual callbacks with ReactJS
// event handlers. Moreover, we'll need to use extensible retention once we
// want to memoize render-calls by introducing custom components.
// h$registerExtensibleRetention(scanTree);
