next
----
* Add `Comonad` instances for `Tagged s` with `s` of any kind. Before the
  change, `s` had to be of kind `*`.

5.0.3 [2018.02.06]
------------------
* Don't enable `Safe` on GHC 7.2.

5.0.2
-----
* Support `doctest-0.12`

5.0.1
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-1.25`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

5
-
* Removed module `Data.Functor.Coproduct` in favor of the `transformers`
  package's `Data.Functor.Sum`. n.b. Compatibility with older versions of
  `transformers` is possible using `transformers-compat`.
* Add `Comonad` instance for `Data.Functor.Sum.Sum`
* GHC 8 compatibility

4.2.7.2
-------
* Compiles warning-free on GHC 7.10

4.2.7.1
-------
* Use CPP

4.2.7
-----
* `Trustworthy` fixes for GHC 7.2

4.2.6
-----
* Re-export `(Data.Functor.$>)` rather than supply our own on GHC 7.8+
* Better SafeHaskell support.
* `instance Monoid m => ComonadTraced m ((->) m)`

4.2.5
-------
* Added a `MINIMAL` pragma to `Comonad`.
* Added `DefaultSignatures` support for `ComonadApply` on GHC 7.2+

4.2.4
-----
* Added Kenneth Foner's fixed point as `kfix`.

4.2.3
-----
* Add `Comonad` and `ComonadEnv` instances for `Arg e` from `semigroups 0.16.3` which can be used to extract the argmin or argmax.

4.2.2
-----
* `contravariant` 1.0 support

4.2.1
-----
* Added flags that supply unsupported build modes that can be convenient for sandbox users.

4.2
---
* `transformers 0.4` compatibility

4.1
---
* Fixed the 'Typeable' instance for 'Cokleisli on GHC 7.8.1

4.0.1
-----
* Fixes to avoid warnings on GHC 7.8.1

4.0
---
* Merged the contents of `comonad-transformers` and `comonads-fd` into this package.

3.1
---
* Added `instance Comonad (Tagged s)`.

3.0.3
-----
* Trustworthy or Safe depending on GHC version

3.0.2
-------
* GHC 7.7 HEAD compatibility
* Updated build system
