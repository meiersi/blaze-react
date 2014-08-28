# ReactJS bindings for GHCJS

These are experimental bindings for ReactJS using GHCJS. The rendering API is
modeled analogously to blaze-html and the application architecture is based on
simply specifying the complete UI state together with the state transitions
that implement the application's logic. 

This purity simplifies both implementation and debugging. For example one can
generically implement a time-machine wrapper that wrapps an application such
that one can go back and inspect all intermediate states. See it in action in
the classic TodoMVC example in `todomvc` directory.


# Building

~~~~
make dev-tools
make build
~~~~


# Known problems

- The versions in the .cabal file are not properly pinned, which might lead to
  build problems.
- The `extra-sources` field is missing some files, which means `cabal sdist`
  won't work.


# Acknowledgements

* The bindings were heavily inspired by Luite Stegemans
  [virtual-dom](https://github.com/ghcjs/ghcjs-vdom) bindings.
* [Alex Sayers](https://github.com/asayers) implemented the Clock and
  TimeMachine examples.
* [Tomas Carnekcy](https://github.com/werehamster) helped debug some nasty
  GHCJS FFI issues.
