<p align="center">
  <img src=".github/kupo.png" height=120 />
</p>

**Kupo** is a lightweight, configurable, **chain-index** for the Cardano blockchain. It synchronizes data from the blockchain according to **patterns** matching addresses present in transaction outputs, to build a **lookup table** from matches to their associated **output references, values and datum hashes.**

# Getting Started

TODO.

# Installation (from source)

```console
$ nix-build -A kupo.components.exes.kupo
```

This produces a statically-linked executable that provides a command-line interface for passing options and commands.

# Alternatives

### cardano-db-sync

### plutus-chain-index

---

<p align="center">
  <a href="CONTRIBUTING.md">:triangular_ruler: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>

<p align="center"><a href="https://github.com/cardanosolutions/kupo/blob/master/LICENSE"><img src=".github/license.svg" alt="license=MPL-2.0" /></a></p>
