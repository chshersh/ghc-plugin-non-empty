# ghc-plugin-non-empty

[![GitHub CI](https://github.com/chshersh/ghc-plugin-non-empty/workflows/CI/badge.svg)](https://github.com/chshersh/ghc-plugin-non-empty/actions)
[![Hackage](https://img.shields.io/hackage/v/ghc-plugin-non-empty.svg?logo=haskell)](https://hackage.haskell.org/package/ghc-plugin-non-empty)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

GHC Compiler Plugin for automatically converting list literals to the
`NonEmpty` type from the `Data.List.NonEmpty` module in `base`.

This plugin checks statically defined list literals and transforms
them into `NonEmpty` lists during compile time. In other words, it
provides compile-time guarantees for non-emptiness checks and allows
the following expression to type-check:

```haskell
portsToListen :: NonEmpty Int
portsToListen = [8000, 8080, 8081]
```

Compare to usage without the plugin:

```haskell
portsToListen :: NonEmpty Int
portsToListen = 8000 :| [8080, 8081]
```

> ℹ️ **DISCLAIMER:** `ghc-plugin-non-empty` is developed and
> maintained in free time by volunteers. The development may continue
> for decades or may stop tomorrow. You can use
> [GitHub Sponsorship](https://github.com/sponsors/chshersh) to support
> the development of this project.

## How to use?

`ghc-plugin-non-empty` is compatible with the following GHC
versions — [supported versions](https://matrix.hackage.haskell.org/#/package/ghc-plugin-non-empty)

In order to start using `ghc-plugin-non-empty` in your project, you'll
need to set it up with these steps:

1. Add the dependency on `ghc-plugin-non-empty` in your project's
   `.cabal` file. For this, you should modify the `build-depends`
   section according to the below section:

   ```haskell
   build-depends:
     , base                 ^>= LATEST_SUPPORTED_BASE
     , ghc-plugin-non-empty ^>= LATEST_VERSION
   ```

2. To use this package, refer to the below example.

   ```haskell
   {-# OPTIONS_GHC -fplugin=GhcPluginNonEmpty #-}

   module Main (main) where

   import Data.List.NonEmpty (NonEmpty)

   exampleList :: NonEmpty Int
   exampleList = [100, 5, 74]

   main :: IO ()
   main = print exampleList
   ```

## For contributors

Check [CONTRIBUTING.md](https://github.com/chshersh/ghc-plugin-non-empty/blob/main/CONTRIBUTING.md)
for contributing guidelines.

To build the project and run the tests, use `cabal`:

```shell
cabal build all
cabal test --enable-tests --test-show-details=direct
```
