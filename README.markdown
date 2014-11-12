# ReactJS bindings for GHCJS

These are experimental bindings for ReactJS using GHCJS. The rendering API is
modeled analogously to blaze-html and the application architecture is based on
simply specifying the complete UI state together with the state transitions
that implement the application's logic.

This purity simplifies both implementation and debugging. For example one can
generically implement a time-machine wrapper which wraps an application such
that one can go back and inspect all intermediate states. To see it in action,
take a look at the classic TodoMVC example in `todomvc` directory, which is
also available [online][1].

[1]: https://meiersi.github.io/blaze-react/

## Building

First of all, make sure you have ghcjs installed (including the patched version
of cabal), as well as nodejs and npm.

Blaze-react depends on [ghcjs-ffiqq][2], which is not on hackage, so you'll
have to install it from github. The suggested way is to run `cabal sandbox
add-source <path to a checkout of ghcjs-ffiqq>`. Once this is done, all
dependencies can be installed with `cabal install --ghcjs --only-dep`.

Now, you can just run

~~~~
make dev-tools
make build
~~~~

[2]: https://github.com/ghcjs/ghcjs-ffiqq

## Known problems

- The versions in the .cabal file are not properly pinned, which might lead to
  build problems.
- The `extra-sources` field is missing some files, which means `cabal sdist`
  won't work.

## Acknowledgements

* The bindings were heavily inspired by Luite Stegeman's
  [virtual-dom](https://github.com/ghcjs/ghcjs-vdom) bindings.
* [Alex Sayers](https://github.com/asayers) implemented the Clock and
  TimeMachine examples.
* [Tomas Carnekcy](https://github.com/werehamster) helped debug some nasty
  GHCJS FFI issues.
