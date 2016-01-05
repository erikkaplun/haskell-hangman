# haskell-hangman

A hangman implementation in Haskell to explore Event Sourcing and FRP

The purpose of this repository is to explore how Event Sourcing
naturally arises when modelling a domain, and how Event Sourcing
(presumably) naturally generalizes to FRP.

It's especially interesting in (pure) functional languages such
as Haskell where operators like `fold` and `scan`, which FRP/ES
build on, are used routinely.
