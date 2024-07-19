<p align="center">
  <img src="./docs/kupo.png" width="240" /> <img src=".github/preview.gif" width="480" />
</p>

<p>
  <img align="left" src=".github/installation.png" width="300" alt="installation instructions" />

  <a href="https://cardanosolutions.github.io/kupo">📖 User Manual</a>
  |
  <a href="CONTRIBUTING.md">🤝 Contributing</a>
  |
  <a href="CHANGELOG.md"> 💾 Changelog</a>
  |
  <a href="https://twitter.com/_KtorZ_"><img src=".github/twitter.svg" alt="Twitter"> Twitter (@_KtorZ_)</a>
</p>

---

**Kupo** is fast, lightweight and configurable **chain-index** for the Cardano blockchain. It synchronizes data from the blockchain according to **patterns** matching addresses present in transaction outputs and builds a **lookup table** from matches to their associated **output references**, **values**, **datums** and **scripts**.


# Roadmap

### Cutting-edge work

The project keeps a [changelog 💾](./CHANGELOG.md) in which all _recent-but-not-yet-released_ changes can be seen. Changes in the changelog have been implemented and are available in latest builds from the `master` branch.

> [!WARNING]
> Work available on the `master` branch shall be considered _unstable_ in the sense that interfaces may slightly change and database migrations may not work from a previous `master` commit. Keeping stability between unreleased commits is only a best-effort.

### Planned work

The [projects boards 🎯][roadmap] list planned tasks that haven't been implemented but have reached enough maturity to be well-defined and scoped.

### Future work

Finally, [discussions 💡][discussions] contains ongoing discussions regarding the future of Kupo, with design decisions still under consideration.

# Compatibility

Kupo     | Cardano-node      | Ogmios   | Hydra
---      | ---               | ---      | ---
`v2.9.*` | `9.0.0`           | `v6.*.*` | `0.16.0`
`v2.8.*` | `8.7.*`           | `v6.*.*` | `0.13.0`
`v2.7.*` | `8.1.*`, `8.6.*`  | `v6.*.*` | `0.13.0`
`v2.6.*` | `8.1.*`           | `v6.*.*` | N/A
`v2.5.*` | `1.35.*`, `8.1.*` | `v5.*.*` | N/A

# System Requirements

| Category         | Value            |
| ---              | ---              |
| Operating System | Linux / MacOS    |
| Architecture     | arm64 / amd64    |
| RAM              | 256MB-2048MB[^1] |
| CPU              | 2+ cores         |
| Disk Storage     | 1MB-220GB[^2]    |

# Continuous Integration

| Status | Description |
| --- | --- |
| <a href="https://hub.docker.com/r/cardanosolutions/kupo"><img src="https://img.shields.io/docker/pulls/cardanosolutions/kupo?style=for-the-badge&label=&logo=Docker&logoColor=000000&color=f9dd24" /></a> | Docker image pulls |
| <a href="https://github.com/CardanoSolutions/kupo/actions/workflows/build.yaml"><img src="https://img.shields.io/github/actions/workflow/status/cardanosolutions/kupo/build.yaml?style=for-the-badge&label=&logo=NixOS&logoColor=000000&color=f9dd24" /></a> | Nix build, providing static binary executables as artifacts. |
| <a href="https://github.com/CardanoSolutions/kupo/actions/workflows/pages/pages-build-deployment"><img src="https://img.shields.io/github/deployments/cardanosolutions/kupo/github-pages?style=for-the-badge&label=&logo=readthedocs&logoColor=000000&color=f9dd24"></a> | [User manual][] and API reference deployment. |
| <img src="https://img.shields.io/static/v1?style=for-the-badge&label=&message=82%&logo=codecov&logoColor=000000&color=f9dd24"> | Test code coverage. Learn more about the [testing strategy][]. |

# Alternatives

Kupo is well-suited for small (or large) applications which need either:

- a global chain index for resolving outputs by address, policy id or output reference;
- a on-the-fly monitoring of an address, policy id or specific transaction output over a short period of time.

It runs in constant memory and is blazing fast. Yet, its use-cases are limited. Here below we provide some possible alternatives with different trade-offs:

| **Solution** | **Ecosystem** | **Key difference(s)** |
| ---          | ---           | ---                   |
| <a href="https://github.com/txpipe/oura/#readme"><img width=150 src="https://github.com/txpipe/oura/raw/main/assets/logo.svg" alt="oura"></a> | Rust | Oura in itself does not provide any chain-indexing, but it supports pluggable sinks where filtered data from the Cardano blockchain can be dumped into (e.g. Elastic Search or Kafka). It also supports a wider variety of events. All-in-all, a good fit for more elaborate solutions. |
| <a href="https://github.com/blinklabs-io/adder"><img width=150 src="https://github.com/blinklabs-io/adder/raw/main/assets/adder-logo-with-text-horizontal.png" alt="adder" /></a> | Go | Similar to Oura, Adder is is an event-emitter coupled with filters. Therefore, it requires a backend storage or an additional solution for handling and serving events coming from the chain. Like Kupo, it supports a variety of filters on addresses or assets albeit in a more direct way. |
| <a href="https://github.com/bloxbean/yaci-store"><img width=150 src="https://github.com/bloxbean/yaci-store/raw/main/static/YaciStore.png" alt=""></a> | Java | Yaci Store is a small component built on top of the Yaci libraries -- a collection of Java libraries for interacting with the Cardano networking protocols. It can be used as a library to deal with an event stream similar to Oura and Adder, or as a full blackbox solution storing into various storage solutions (SQLite, Redis, MongoDB, ...). |
| <a href="https://github.com/txpipe/scrolls"><img width=150 src="https://github.com/txpipe/scrolls/raw/main/assets/logo-dark.png?sanitize=true#gh-dark-mode-only" alt="scrolls"></a> | Rust | Scrolls provides (at this stage) an in-memory aggregation engine via Redis. It allows applications to watch and react instantly on the evolution of some aggregated metric (see Scrolls' README for details about what metrics are supported). Because the data is fully stored in-memory, it is not possible to index the entire chain without resorting to large memory requirements. Hence it is more tailored to specific handpicked pieces of information. It also synchronizes blocks from the chain using the node-to-node protocol which means that it can do so on any remote node relay, but it is also slower (because a more defensive protocol) than the node-to-client protocol upon which Kupo relies. |
| <a href="https://dcspark.github.io/carp/docs/intro/"><img width=150 src="https://dcspark.github.io/carp/img/logo.svg" alt="carp"/></a> | Rust | Carp is a modular blockchain indexer built on top of Oura; it synchronizes data in a PostgreSQL database based on behaviors described in _tasks_ (Rust standalone plugins). Some pre-defined common tasks are already available, other can be written on-demand to fit one's use case. As a primary interface, Carps fully relies on PostgreSQL. |
| <a href="https://github.com/input-output-hk/cardano-db-sync#cardano-db-sync">cardano-db-sync</a> | Haskell | cardano-db-sync synchronizes ALL data from the Cardano blockchain, whereas Kupo focuses only on transaction outputs. This comes with obvious trade-offs in both on-disk storage, runtime requirements and performances. Kupo is usually an order of magnitude faster for retrieving outputs by address, stake address or policy id. Note also that like Carp, cardano-db-sync's primary interface is a PostgreSQL database whereas Kupo offers a higher-level HTTP API over JSON.

## Sponsors

A big thank to [all our sponsors 💖](https://github.com/CardanoSolutions#-sponsors).

[testing strategy]: https://github.com/CardanoSolutions/kupo/tree/master/test#testing-strategy
[user manual]: https://cardanosolutions.github.io/kupo
[discussions]: https://github.com/CardanoSolutions/kupo/discussions/categories/ideas?discussions_q=category%3AIdeas+sort%3Atop
[roadmap]: https://github.com/CardanoSolutions/kupo/projects?type=classic

[^1]: The maximum memory usage depends on runtime flags and internal settings. This can be made lower if necessary (possibly as small as a hundred megabytes) by tweaking those settings. The obvious counter-part being slower synchronization times when syncing over large chunks of data. When synchronized, however, this has close to no impact.

[^2]: The size of the database depends on the patterns the indexer is configured with. Storing every single entry of Mainnet currently come close to ~220GB. However, if pruned (i.e. only keep unspent entries), Mainnet is around 12GB.
