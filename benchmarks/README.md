# Kupo Benchmarks

This directory contains scripts that generate Kupo benchmarks, along with
latest results.

## Latest results

* [mainnet](./mainnet.md) benchmarks
* [preprod](./preprod.md) benchmarks
* [comparison](./comparison.md) of the last two versions

## Generating benchmarks

### Requirements

- [`oha`][oha] and `runghc` must be available in PATH.
- A Cardano node (mainnet or preprod accordingly) must be running locally and
  sync'ed.
- A Kupo instance (the one to be benchmarked) must be running locally and
  connected to the running Cardano node. It must have been started with these
  flags: `--since origin --defer-db-indexes --prune-utxo --workdir <dir>
  --match "*"` and have been given time to roll forward and generate indexes.

### Instructions

- [bench](./bench) is a Bash script that generates benchmark results
- [stats.hs](./stats.hs) is a Haskell script that performs statistical
  comparisons of two benchmark data sets.

There are three options for running the `bench` script:

1. `./bench mainnet` benchmarks Kupo against mainnet and prints graphical
   results to the terminal.
2. `./bench preprod` benchmarks Kupo against preprod and prints graphical
   results to the terminal.
3. `./bench data` benchmarks Kupo against preprod and writes CSV output to
   files for [comparative analysis](#comparative-analysis)

### Comparative analysis

It is possible to compare two versions of Kupo. This is used by the developers
to compare a future version with the last version.

The process is as follows:

1. Run a preprod Cardano node
1. Start the control version of Kupo (typically last release, see above for
   flags). For example, if Kupo and Cardano node were installed from the
   [notunrandom/cardano][tap] Homebrew tap, just invoke `kupo` with the
   required arguments:
   ```
   kupo --node-socket $(brew --prefix)/var/cardano/preprod/node.socket --node-config $(brew --prefix)/etc/cardano/preprod/config.json --since origin --defer-db-indexes --prune-utxo --workdir ~/kupodb --match "*"
   ```
1. Wait for Kupo to finish rolling forward and create indexes.
1. Run `./bench data`. This generates an `./index` file, a `./data`
   subdirectory and a timestamped subdirectory in `./data` containing numbered
   files with benchmark results. Each number corresponds to one of the queries
   used. The `index` files gives the query corresponding to each number. These
   queries come from the [bench](./bench) script.
1. Stop the control version of Kupo and start the experimental version of
   Kupo (typically compiled from the main branch or a branch on which a release
   is being prepared). Generally this would mean invoking `$(cabal list-bin
   exe:kupo)`, from inside the cloned kupo repository with the same arguments
   as the previous run. For example, using the same node as above and same
   `workdir` for Kupo (assuming the versions use the same database):
   ```
   $(cabal list-bin exe:kupo) --node-socket $(brew --prefix)/var/cardano/preprod/node.socket --node-config $(brew --prefix)/etc/cardano/preprod/config.json --since origin --defer-db-indexes --prune-utxo --workdir ~/kupodb --match "*"
   ```
1. Wait for Kupo to finish rolling forward and create indexes.
1. Run `./bench data` again.
5. Run `runghc stats`. This provides results such as:

```
Comparing:
data/20260415-124419 (s1: experimental group)
data/20260415-123135 (s2: control group)
Common datasets:
1 2 3 4 5 6 7 8
Significance level: α = 0.05
1: errors (s1/s2): (0/0); comparison: not significant
2: errors (s1/s2): (0/0); comparison: not significant
3: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0158072916394956
4: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.031525671195153
5: errors (s1/s2): (0/0); comparison: s1 faster by factor 0.9762969980218498
6: errors (s1/s2): (0/0); comparison: not significant
7: errors (s1/s2): (0/0); comparison: not significant
8: errors (s1/s2): (0/0); comparison: s1 faster by factor 7.015094471488993e-5

```

The errors give the count of HTTP return codes other than 200 (out of 110
requests).

The comparison:
- compares 100 samples for the response delay (in seconds) of each query.
- requires at least 100 (out of 110 runs) succesfull requests (both control and
  experimental), otherwise is considered not significant.
- Performs Mann-Whitney-Wilcoxon rank sum comparison on 100 samples. If the
  difference in sums is statistically significant, the ratio of mean response
  delays (calculated using the numerically stable cumulative Welford algorithm)
  is provided.

[oha]: https://github.com/hatoo/oha
[tap]: https://github.com/notunrandom/homebrew-cardano
