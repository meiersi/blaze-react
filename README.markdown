# blaze-react - blazingly fast development of reactive applications

...at least that's what we are working towards ;-)


## History

The blaze-react project has its roots in an experiment of using GHCJS together
with React.js to build single-page web-applications in Haskell. See this

[demo]: https://meiersi.github.io/blaze-react/

for a show-case of the resulte, whose code can be found on the following
branches.

- old-master: the latest released version
- old-rc-v0.2: the last release candidate of the old framework

This work was done in an on-and-off fashion and yielded many of the insights
for how we'd like our first production-ready version to be structured. We
describe the ideas and the work left to be done in the following roadmap.


## Roadmap

- describe current module layout, and what should go where (core, rendering,
  backends).
- describe current library layout
- explain that we do not plan to support existing JS widgets, as we want our
  application code to be platform independent
- explain focus on improved-base GHCJS only


### Stage 1 - Correctness

### Write tests, in particular webrunner-based tests.

Whether all features of the blaze-react React.js backend are working or not is
currently very hard to judge. Tests based on web-runner are essential, and we
should copy the approaches put forward by `react-flux`. I imagine that we want
to automate the testing using `nix`.


### Memory-leak-free callbacks

We currently allocate callbacks such that they are retained forever. This is
wrong.

GHCJS has its own garbage collector, which is necessary to implement
weak references. This means that callbacks must be allocated and deallocated
explicitly, or we must enable the GC to traverse the ReactJS objects. I'd
suggest that we look into how react-flux and reflex are handling this issue,
and then draw up a plan. Ideally, it is such that we come up with a solution
where we can freely allocate rendering and event-handling callbacks.

As part of this move I also suggest to port our code to the newest React.js
version, in particular if we build a custom traversal of the React.js
components.


### Components and lazy rendering

We currently do not support lazy rendering at all. This will not work for
applications that require a large number of DOM nodes.

- TODO (SM): spell out the plan that I have on how to implement components
  with typeclasses such that we can statically guarantee the correctness of
  the lazy rendering.

```.haskell
class WeakEq a where
    -- | A weak equality satisfying the property that
    -- @forall x y. weakEq x y ==> x == y@.
    weakEq :: a -> a -> Bool

class Typeable props => Component props where
    type Rendered props :: *

    render :: props -> Rendered props

    shouldUpdate :: props -> props -> Bool

    default shouldUpdate :: WeakEq props => props -> props -> Bool
    shouldUpdate = weakEq
```


### Resource management in IO requests

Applications must execute IO actions to communicate with the outside world.
The life-time of such requests must be bound to the life-time of the
application itself. We must provide support to implement that properly. It
will probably look about as follows.

```.haskell
-- | State transitions of an application with state @st@ and requests @req@.
type Transition st req a = StateT st (Writer [req]) a

-- | An IO monad that allows initiating state transitions
newtype IORequest st a = IORequest
    { runIORequest
        :: ResourceManager
           -- ^ a resource-t like manager for safely allocating
           -- resources like timeouts and threads
        -> (Transition st (IORequest st ()) () -> IO ())
           -- ^ A callback that can be used to submit state transitions to the
           -- application.
        -> IO a
    }

-- | An application runner that executes
runAppReactJs
  :: st                                             -- initial state
  -> IORequest st ()                                -- initial request
  -> (st -> Html (EventHandler (IORequest st ())))  -- rendering
  -> IO ()

-- | Run an application on the server-side to prefetch its data before sending
-- a pre-initialized state to the client-side.
--
-- We can use this in combination with the rendering of a 'Html' value to a
-- string to provide very fast initial-page-load times for applications that
-- need to fetch data.
--
-- Note that the implementation of this function relies on the ability to
-- forcefully de-allocate all resources used by 'IORequest's.
prefetchApp
  :: st                 -- ^ initial state
  -> IORequest st ()    -- ^ initial request
  -> (st -> Bool)
     -- ^ A predicate that states when the state is complete enough, i.e.,
     -- when enough data has been prefetched.
  -> IO st
```

As hinted at in the example above, we want to have that support, as we want to
be able to execute applications on the server-side (e.g., for testing,
development and page pre-rendering). There, we must make sure that we do not
leak threads or other resources.



### Stage 2 - Completeness

- support for all React.js events and properties
- path and window title interaction
- basic performance optimizations: minimize marshaling costs
- development mode with auto server-restart (TODO: explain that one can
  already run a blaze-react application from GHCi. Only the auto-restart is
  missing.)


### Version 1.0

- documentation and additional code supporting common  architectural patterns
  (widgets, caching, data fetching).
- lucid-based html combinators
- fast server-side rendering


### Version 2.0

- inline styles


## Building

The easiest way to build the libraries for GHCJS is using `nix`. All
derivations for the libraries are exposed from `default.nix` in the attribute
`libs`.

Note that we need a recent version of the improved-base branch of GHCJS.
You can enter development shells for the individual packages like
`blaze-react-spa` with such a recent version exposed using the following
nix-shell invocation in the root of the repository.

```
nix-shell '.'  -A 'assembled.ghcjs.blaze-react-spa.env'
```

## Acknowledgements

* The bindings were heavily inspired by Luite Stegeman's [virtual-dom][]
  bindings.
* [Tomas Carnecky][] helped debug some nasty GHCJS FFI issues.

[virtual-dom]: https://github.com/ghcjs/ghcjs-vdom
[Tomas Carnekcy]: https://github.com/werehamster
