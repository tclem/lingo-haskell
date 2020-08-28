FROM haskell:8.6 as build
WORKDIR /build

COPY . .
RUN cabal new-update hackage.haskell.org,HEAD
RUN cabal new-configure
RUN cabal new-build
RUN cabal new-test
