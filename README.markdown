# `blaze-react` v0.2

This is an experimental library for writing highly portable applications. It
allows you to express the pure logic of your application in a way which is
completely independent of the execution context.

This means that the same app could be compiled to a client-side web app by
GHCJS for snappy performance, or to a server-side web app in order to support
old browsers. An app written for the command-line could be given a web
interface, or a GUI using OpenGL or GTK. The logic of the app doesn't change -
only the rendering code needs to be updated.

See the `Blaze.Core.App` datatype for the fundamental abstraction, and see
`Blaze.ReactJS.Run.runApp'` for an example of how these `App`s can be used. To
see the whole thing in action, take a look in the `example-app/` directory.

This abstraction has some other benefits. It makes it possible to write some
cool "app transformers", such as the Time Machine, which records the internal
state of an app as it runs, and allows the user to scrub backwards and forwards
through time, replaying the app's behaviour. [See it in action][demo].

It should also make it possible to write property-based tests for your
application as a whole, because in general the set of possible user interations
is enumerable.

[demo]: https://meiersi.github.io/blaze-react/

Right now there's only one way to run your `App`s: as client-side web-apps.
This library provides an API for writing HTML-based UIs which is modeled on
`blaze-html`. The DOM is then updated using react.js. The name of this library
is due to these implementation details.


## Building

First of all, make sure you have GHCJS installed, as well as nodejs and npm.

Blaze-react depends on `ghcjs-ffiqq`, which is not on hackage, so you'll have
to [grab it from github][ffiqq]. Then either install it globally, or (better)
point your local sandbox to the checkout with `cabal sandbox add-source <path
to checkout>`. Once this is done, you need to do a bit of fairly standard
set-up, and then you can build the library:

```
make dev-tools
cabal install --only-dep
cabal configure
make build
```

[ffiqq]: https://github.com/ghcjs/ghcjs-ffiqq

If you want to build the example app as well (found in the `example-app/` directory)
then you should configure the project with the `build-example` flag set:

```
cabal configure -fbuild-example
make build
open todomvc/index.html
```

## Known problems

- The `extra-sources` field is missing some files, which means `cabal sdist`
  won't work.

## Acknowledgements

* The bindings were heavily inspired by Luite Stegeman's [virtual-dom][]
  bindings.
* [Tomas Carnekcy][] helped debug some nasty GHCJS FFI issues.

[virtual-dom]: https://github.com/ghcjs/ghcjs-vdom
[Tomas Carnekcy]: https://github.com/werehamster
