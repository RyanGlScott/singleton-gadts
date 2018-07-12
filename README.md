# `singleton-gadts`
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/RyanGlScott/singleton-gadts.svg)](https://travis-ci.org/RyanGlScott/singleton-gadts)

[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package provides a variant of the `SingKind` class from the `singletons` library which supports GADTs and poly-kinded data types, as well as Template Haskell functionality to quickly generate instances of it.

The design of this library is heavily inspired by code written [here](https://github.com/goldfirere/singletons/issues/150#issuecomment-306539091) by Richard Eisenberg. This technique is presented as a separate library (instead of incorporating the technique directly into the `singletons` library) due to some annoying steps one must take to work around [GHC Trac #12088](https://ghc.haskell.org/trac/ghc/ticket/12088), which affects the code here. Refer to the documentation in the `Data.Singletons.GADT` module for more information.
