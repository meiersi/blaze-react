# Convenient re-exports of the build functions for the individual cabal
# packages constituting blaze-react.

{
  libs = {
    blaze-react-core     = import libs/hs/blaze-react-core;
    blaze-react-spa      = import libs/hs_js/blaze-react-spa;
    blaze-react-examples = import libs/hs_js/blaze-react-examples;
  };
  apps = {
    blaze-react-demo = import apss/hs_js/blaze-react-example;
  };
}
