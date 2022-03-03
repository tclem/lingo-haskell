[![Build Status](https://github.com/tclem/lingo-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/tclem/lingo-haskell/actions) [![hackage](https://img.shields.io/hackage/v/lingo.svg?color=blue&style=popout)](http://hackage.haskell.org/package/lingo)

# Lingo

Detect programming languages from file extensions and common filenames. Based on [linguist's](https://github.com/github/linguist) registry of languages.

## Development

```
cabal v2-update
cabal v2-configure
cabal v2-build
```

You can also use the [Bazel](https://haskell.build) build system:

```
bazel build //...
```
