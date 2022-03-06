<p align="center">
  <img src="./docs/kupo.png" height=300 width=456 />
  <hr/>
  <p align="center">
  <a href="https://hub.docker.com/r/cardanosolutions/kupo"><img src="https://img.shields.io/github/workflow/status/cardanosolutions/kupo/Docker?style=for-the-badge&label=&logo=Docker&logoColor=FFFFFF" /></a> <a href="https://github.com/CardanoSolutions/kupo/actions/workflows/nix.yaml"><img src="https://img.shields.io/github/workflow/status/cardanosolutions/kupo/Nix?style=for-the-badge&label=&logo=NixOS&logoColor=FFFFFF" /></a>
  </p>
</p>

**Kupo** is fast, lightweight and configurable **chain-index** for the Cardano blockchain. It synchronizes data from the blockchain according to **patterns** matching addresses present in transaction outputs and builds a **lookup table** from **matches** to their associated **output references**, **values** and **datum hashes**.

# Getting Started

See the [user-manual 🕮 ](https://cardanosolutions.github.io/kupo).

# System Requirements

| Category         | Value                               |
| ---              | ---                                 |
| Operating System | Linux 64-bit                        |
| RAM              | 256MB                               |
| CPU              | 2 cores                             |
| Disk Storage     | 0 GB (in memory) - 4GB (full index) |

# Alternatives

Kupo is well-suited for small applications which need either: 

- a global chain index for resolving output references;
- a on-the-fly monitoring of an address over a short period of time.

It runs in constant memory and is blazing fast. Yet, its use-cases are limited. Here below we provide some possible alternatives with different trade-offs:

<details>
  <summary>oura</summary>

Key difference(s): Oura in itself does not provide any chain-indexing, but it supports pluggable sinks where filtered data from the Cardano blockchain can be dumped into (e.g. Elastic Search or Kafka). It also supports a wider variety of events. All-in-all, a good fit for more elaborate solutions.

<p align="right">
  <a href="https://github.com/txpipe/oura/#readme">Learn more</a>
  </p>
</details>


<details>
  <summary>cardano-db-sync</summary>

Key difference(s): cardano-db-sync synchronizes ALL data from the Cardano blockchain, whereas Kupo focuses only on transaction outputs. This comes with obvious trade-offs in both on-disk storage but also runtime requirements. 

<p align="right">
  <a href="https://github.com/input-output-hk/cardano-db-sync#cardano-db-sync">Learn more</a>
</p>
</details>

<details>
  <summary>plutus-chain-index</summary>

Key differences(s): the plutus-chain-index is the native component behind the PAB (Plutus Application Backend). It is however intended to be user-facing and as such, does not provide a friendly user experience for uses outside of the PAB's internals.

<p align="right">
  <a href="https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index-core#plutus-chain-index">Learn more</a>
</p>
</details>

## Sponsors :heart:

<p align="center">
  <a href="https://rraayy.com/"><img src="https://avatars.githubusercontent.com/u/65092852?s=55&v=4" width=55 height=55 /></a>
  <a href="https://sundaeswap.finance/"><img src="https://avatars.githubusercontent.com/u/83610786?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/savaki"><img src="https://avatars.githubusercontent.com/u/108710?s=55&v=4" width=55 height=55 /></a>
  <a href="https://blockfrost.io/"><img src="https://avatars.githubusercontent.com/u/70073210?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/jacoblambda"><img src="https://avatars.githubusercontent.com/u/9424043?s=55&v=4" width=55 height=55 /></a>
  <a href="https://ccvault.io/"><img src="https://avatars.githubusercontent.com/u/86010408?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/codybutz"><img src="https://avatars.githubusercontent.com/u/3670430?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/scarmuega"><img src="https://avatars.githubusercontent.com/u/653886?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/minswap"><img src="https://avatars.githubusercontent.com/u/80548193?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/mrbrinker"><img src="https://avatars.githubusercontent.com/u/41247403?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/artemwright"><img src="https://avatars.githubusercontent.com/u/83517471?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/kayandra"><img src="https://avatars.githubusercontent.com/u/5002506?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/tapiocapool"><img src="https://avatars.githubusercontent.com/u/80033713?s=55&v=4" width=55 height=55 /></a>
</p>

---

<p align="center">
  <a href="https://cardanosolutions.github.io/kupo">🕮 User Manual</a>
  |
  <a href="CONTRIBUTING.md"> 📐 Contributing</a>
  |
  <a href="CHANGELOG.md"> 💾 Changelog</a>
</p>

<p align="center"><a href="https://github.com/cardanosolutions/kupo/blob/master/LICENSE"><img src=".github/license.svg" alt="license=MPL-2.0" /></a></p>
