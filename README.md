<p align="center">
  <img src="./docs/kupo.png" height=210 width=320 />
</p>

<hr/>

**Kupo** is fast, lightweight and configurable **chain-index** for the Cardano blockchain. It synchronizes data from the blockchain according to **patterns** matching addresses present in transaction outputs and builds a **lookup table** from matches to their associated **output references**, **values**, **datums** and **scripts**. 

<p align="center">
  <img src="./docs/architecture-diagram.png" />
</p>

# Getting Started

See the [user-manual 📖](https://cardanosolutions.github.io/kupo).

# Roadmap 

See [projects 🎯](https://github.com/CardanoSolutions/kupo/projects?type=classic).

# System Requirements

| Category         | Value                                     |
| ---              | ---                                       |
| Operating System | Linux 64-bit                              |
| RAM              | 1280MB<sup>1</sup>                        |
| CPU              | 2 cores                                   |
| Disk Storage     | Variable (few MB, up to 40GB)<sup>2</sup> |

> <sup><strong>1</strong></sup> The maximum memory usage depends on runtime flags and internal settings. This can be made lower if necessary (possibly as small as a hundred megabytes) by tweaking those settings. The obvious counter-part being slower synchronization times when syncing over large chunks of data. When synchronized, however, this has close to no impact. 

> <sup><strong>2</strong></sup> The size of the database depends on the patterns the indexer is configured with. Storing every single entry of Mainnet currently come close to ~40GB. However, if pruned (i.e. only keep unspent entries), Mainnet is around 4GB.
> 

# Continuous Integration

| Status | Description | 
| --- | --- | 
| <a href="https://github.com/CardanoSolutions/kupo/actions/workflows/docker.yaml"><img src="https://img.shields.io/github/workflow/status/cardanosolutions/kupo/Docker?style=for-the-badge&label=&logo=Docker&logoColor=000000&color=f9dd24" /></a> | Docker build, shipping images to [Dockerhub](https://hub.docker.com/r/cardanosolutions/kupo) |
| <a href="https://github.com/CardanoSolutions/kupo/actions/workflows/nix.yaml"><img src="https://img.shields.io/github/workflow/status/cardanosolutions/kupo/Nix?style=for-the-badge&label=&logo=NixOS&logoColor=000000&color=f9dd24" /></a> | Nix build, providing static binary executables as artifacts. | 
| <a href="https://github.com/CardanoSolutions/kupo/actions/workflows/pages/pages-build-deployment"><img src="https://img.shields.io/github/deployments/cardanosolutions/kupo/github-pages?style=for-the-badge&label=&logo=readthedocs&logoColor=000000&color=f9dd24"></a> | [User manual][] and API reference deployment. |
| <img src="https://img.shields.io/static/v1?style=for-the-badge&label=&message=90%&logo=codecov&logoColor=000000&color=f9dd24"> | Test code coverage. Learn more about the [testing strategy][]. |

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
  <summary>scrolls</summary>

Key differences(s): Scrolls provides (at this stage) only an in-memory storage via Redis. This means that it's not possible to index the entire chain without resorting to large memory requirements. It also synchronizes blocks from the chain using the node-to-node protocol which means that it can do so on any remote node relay, but it is also slower (because a more defensive protocol) than the node-to-client protocol upon which Kupo relies. 

<p align="right">
  <a href="https://github.com/txpipe/scrolls">Learn more</a>
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

## Sponsors 💖 

<p align="center">
  <a href="https://rraayy.com/"><img src="https://avatars.githubusercontent.com/u/65092852?s=45&v=4" width=45 height=45 /></a>
  <a href="https://sundaeswap.finance/"><img src="https://avatars.githubusercontent.com/u/83610786?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/savaki"><img src="https://avatars.githubusercontent.com/u/108710?s=45&v=4" width=45 height=45 /></a>
  <a href="https://blockfrost.io/"><img src="https://avatars.githubusercontent.com/u/70073210?s=45&v=4" width=45 height=45 /></a>
  <a href="https://jpeg.store/"><img src="https://avatars.githubusercontent.com/u/98781883?s=200&v=4" width=45 height=45 /></a>
  <a href="https://github.com/jacoblambda"><img src="https://avatars.githubusercontent.com/u/9424043?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/minswap"><img src="https://avatars.githubusercontent.com/u/80548193?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/Quantumplation"><img src="https://avatars.githubusercontent.com/u/49870?v=4" width=45 height=45 /></a>
  <a href="https://github.com/codybutz"><img src="https://avatars.githubusercontent.com/u/3670430?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/scarmuega"><img src="https://avatars.githubusercontent.com/u/653886?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/mrbrinker"><img src="https://avatars.githubusercontent.com/u/41247403?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/sacrelege"><img src="https://avatars.githubusercontent.com/u/7289595?v=4" width=45 height=45 /></a>
  <a href="https://ccvault.io/"><img src="https://avatars.githubusercontent.com/u/86010408?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/artemwright"><img src="https://avatars.githubusercontent.com/u/83517471?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/kayandra"><img src="https://avatars.githubusercontent.com/u/5002506?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/tapiocapool"><img src="https://avatars.githubusercontent.com/u/80033713?s=45&v=4" width=45 height=45 /></a>
</p>

---

<p align="center">
  <a href="https://cardanosolutions.github.io/kupo">📖 User Manual</a>
  |
  <a href="CONTRIBUTING.md"> 📐 Contributing</a>
  |
  <a href="CHANGELOG.md"> 💾 Changelog</a>
  |
  <a href="https://twitter.com/_KtorZ_"><img src=".github/twitter.svg" alt="Twitter"> Twitter (@_KtorZ_)</a>
</p>

<p align="center"><a href="https://github.com/cardanosolutions/kupo/blob/master/LICENSE"><img src=".github/license.svg" alt="license=MPL-2.0" /></a></p>

[testing strategy]: https://github.com/CardanoSolutions/kupo/tree/master/test#testing-strategy
[user manual]: https://cardanosolutions.github.io/kupo
