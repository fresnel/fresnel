# `fresnel`: high-powered optics in a small package

[![CI status badge](https://github.com/fresnel/fresnel/actions/workflows/ci.yml/badge.svg)][CI status] [![hackage](https://img.shields.io/hackage/v/fresnel.svg?color=blue&style=popout)][hackage]

`fresnel` is yet another optics (aka functional references) package for Haskell.

[CI status]: https://github.com/fresnel/fresnel/actions
[hackage]: http://hackage.haskell.org/package/fresnel


[![diagram of the optical and profunctor hierarchies and their relationships to one another](https://raw.githubusercontent.com/fresnel/fresnel/main/docs/optics.svg)](https://antitypical.com/fresnel/index.html)

_The above image is interactive. Click to open it, and then hover over labels to highlight everything they include._


## The name

Fresnel (pronounced approx. like “fray knell”) lenses are compact optics commonly seen in lighthouses, allowing much greater brightness or magnification than would otherwise be possible due to the size and weight of conventional glass optics.

It seemed like an apt metaphor for a pared-down optics library based on functions and using `ghc`’s constraint solver to do the heavy lifting.


## Comparisons

Like `optics`, it uses profunctor optics rather than the Van Laarhoven representation. Like `lens`, it uses type synonyms and `-XRankNTypes` to compose optics with the `.` operator and allow `ghc` to deduce the (faux-)subtyping relationships between optics.

Unlike `lens` (but like `optics`), it goes to some effort to provide a sensibly-named hierarchy of optics to improve type errors. In `optics` this is provided by means of type families providing the subsumption relation and constraints; in `fresnel`, names, subsumption, and constraints are all provided by `Is*` typeclasses (e.g. `IsPrism`, `IsTraversal`, etc.) subclassing the profunctor classes directly.

Unlike `lens`, it doesn’t define any typeclasses to abstract over optics’ sources. It also has a comparatively minimal suite of combinators for complex compositions of projections, etc.

Unlike both `lens` _and_ `optics`, `fresnel` (currently) has no support for indexed optics, or certain other baroque optics. Some of these would be simpler additions than others, and are likely to be added in the fullness of time.

Finally, and also unlike either, `fresnel` requires a reasonably small set of dependencies: `base`, `containers`, `profunctors`, `transformers`, and `unordered-containers`.
