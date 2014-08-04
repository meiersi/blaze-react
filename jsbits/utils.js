/* temp utils, merge into ghcjs-prim */

function h$mkUnique(o) {
    return (typeof o === 'number')?o:(o === h$unbox_e)?o.d1:h$makeStableName(o);
}
