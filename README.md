# `fresnel`: high-powered optics in a small package

[![CI status badge](https://github.com/robrix/fresnel/actions/workflows/ci.yml/badge.svg)][CI status] [![hackage](https://img.shields.io/hackage/v/fresnel.svg?color=blue&style=popout)][hackage]

`fresnel` is yet another optics (aka functional references) package for Haskell.

[CI status]: https://github.com/robrix/fresnel/actions
[hackage]: http://hackage.haskell.org/package/fresnel


![diagram of the optical and profunctor hierarchies and their relationships to one another](https://raw.githubusercontent.com/robrix/fresnel/thought-dot%2C-fought-dot%2C-shot-dot/docs/optics.svg)


## The name

Fresnel (pronounced approx. like “fray knell”) lenses are compact optics commonly seen in lighthouses, allowing much greater brightness or magnification than would otherwise be possible due to the size and weight of conventional glass optics.

It seemed like an apt metaphor for a pared-down optics library based on functions and using `ghc`’s constraint solver to do the heavy lifting.


## Comparisons

Like `optics`, it uses profunctor optics rather than the Van Laarhoven representation. Like `lens`, it uses type synonyms and `-XRankNTypes` to compose optics with the `.` operator and allow `ghc` to deduce the (faux-)subtyping relationships between optics.

Unlike `optics` (but like `lens`), since it uses functions instead of newtypes, it could suffer worse type errors. (This has not yet been thoroughly explored.)

Unlike `lens`, it doesn’t define any typeclasses to abstract over optics’ sources. It also has a comparatively minimal suite of combinators for complex compositions of projections, etc.

Unlike both `lens` _and_ `optics`, `fresnel` (currently) has no support for indexed optics, or certain other baroque optics. Some of these would be simpler additions than others, and are likely to be added in the fullness of time.

Finally, and also unlike either, `fresnel` offers a minimal dependency graph: just `base`, `profunctors`, and `transformers`.
