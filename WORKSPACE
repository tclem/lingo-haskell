# Give your project a name. :)
workspace(name = "lingo")

# Load the repository rule to download an http archive.
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    sha256 = "78d017aa732b430c0681fff4514503af78a8d8c44df165e603a9433745b16e5e",
    strip_prefix = "rules_haskell-abaec6502a4474f10b3c367fb5e90173ee0e349c",
    urls = ["https://github.com/tweag/rules_haskell/archive/abaec6502a4474f10b3c367fb5e90173ee0e349c.tar.gz"],
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains(version = "8.8.3")

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    packages = [
        "aeson",
        "raw-strings-qq",
        "yaml",
    ],
    snapshot = "lts-16.11",
)
