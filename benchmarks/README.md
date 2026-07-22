# Kupo Benchmarks

This directory contains scripts that generate Kupo benchmarks.

- [bench](./bench) is a Bash script that generates benchmark results
- [stats.hs](./stats.hs) is a Haskell script that performs statistical
  comparisons of two benchmark data sets.

There are three options for running the `bench` script:

1. `./bench mainnet` benchmarks Kupo against [mainnet](#mainnet) and prints
   graphical results to the terminal.
2. `./bench preprod` benchmarks Kupo against [preprod](#preprod) and prints
   graphical results to the terminal.
3. `./bench data` benchmarks Kupo against preprod and writes CSV output to
   files for [comparative analysis](#comparative-analysis)

Latest results:

* [mainnet](#mainnet) benchmarks
* [preprod](./preprod.md) benchmarks
* comparison of the [last two versions](#last-two-versions)

## Requirements

- [`oha`][oha] and `runghc` must be available in PATH.
- A Cardano node (mainnet or preprod accordingly) must be running locally and
  sync'ed.
- A Kupo instance (the one to be benchmarked) must be running locally and
  connected to the running Cardano node. It must have been started with these
  flags: `--since origin --defer-db-indexes --prune-utxo --workdir <dir>
  --match "*"` and have been given time to roll forward and generate indexes.

## Mainnet

- Hardware: MacBook Pro (2021), Apple M1 Max (16-core), 32 GB
- Version: 2.8.0

### Parameters

Benchmarks are conducted on a local Kupo instance, using [`oha`][oha] with the
following parameters:

- Concurrent clients: 8
- Total requests: 30

### Dataset

The data source used for the benchmarks is the pruned mainnet database matching on `*` from genesis until around slot = 115,816,544 (~ Feb 8th, 2024)
It contains a grand total of 11,406,779 indexed outputs, 91,641 unique token policies and 13,592,035 datums.

### Results

#### stake1uxqh9rn76n8nynsnyvf4ulndjv0srcc8jtvumut3989cqmgjt49h6

`[default]`

<table>
<tr>
<td>
<pre>
Total results:    315400
Total data:     5.58 GiB
<br/>
Slowest:        6.0493 s
Fastest:        4.4276 s
Average:        5.3138 s
<br/>
50% within:     5.4097 s
95% within:     5.9854 s
</pre>
</td>
<td>
<pre>
  4.428 [1] |■■■■
  4.590 [5] |■■■■■■■■■■■■■■■■■■■■
  4.752 [0] |
  4.914 [0] |
  5.076 [0] |
  5.238 [1] |■■■■
  5.401 [8] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  5.563 [7] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  5.725 [3] |■■■■■■■■■■■■
  5.887 [2] |■■■■■■■■
  6.049 [3] |■■■■■■■■■■■■
</pre>
</td>
</tr>
</table>

`?spent_after=98245654`

<table>
<tr>
<td>
<pre>
Total results:        9846
Total data:     196.12 MiB
<br/>
Slowest:          0.7108 s
Fastest:          0.5789 s
Average:          0.6686 s
<br/>
50% within:       0.6824 s
95% within:       0.7072 s
</pre>
</td>
<td>
<pre>
0.579 [1] |■■■■
0.592 [0] |
0.605 [5] |■■■■■■■■■■■■■■■■■■■■
0.618 [0] |
0.632 [0] |
0.645 [0] |
0.658 [0] |
0.671 [3] |■■■■■■■■■■■■
0.684 [7] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■
0.698 [8] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
0.711 [6] |■■■■■■■■■■■■■■■■■■■■■■■■
</pre>
</td>
</tr>
</table>

#### 1d7f33bd23d85e1a25d87d86fac4f199c3197a2f7afeb662a0f34e1e.\*

`[default]`

<table>
<tr>
<td>
<pre>
Total results:     54921
Total data:     1.98 GiB
<br/>
Slowest:        2.7960 s
Fastest:        2.2751 s
Average:        2.5943 s
<br/>
50% within:     2.6127 s
95% within:     2.7804 s
</pre>
</td>
<td>
<pre>
2.275 [1] |■■■■
2.327 [5] |■■■■■■■■■■■■■■■■■■■■
2.379 [0] |
2.431 [0] |
2.483 [0] |
2.536 [0] |
2.588 [3] |■■■■■■■■■■■■
2.640 [8] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
2.692 [3] |■■■■■■■■■■■■
2.744 [3] |■■■■■■■■■■■■
2.796 [7] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■
</pre>
</td>
</tr>
</table>

`?created_before=98245654&spent_after=98764054`

<table>
<tr>
<td>
<pre>
Total results:       184
Total data:     8.16 MiB
<br/>
Slowest:        1.2767 s
Fastest:        1.0572 s
Average:        1.2034 s
<br/>
50% within:     1.2329 s
95% within:     1.2587 s
</pre>
</td>
<td>
<pre>
1.057 [1]  |■■
1.079 [5]  |■■■■■■■■■■■■■
1.101 [0]  |
1.123 [0]  |
1.145 [0]  |
1.167 [0]  |
1.189 [0]  |
1.211 [0]  |
1.233 [9]  |■■■■■■■■■■■■■■■■■■■■■■■■
1.255 [12] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
1.277 [3]  |■■■■■■■■
</pre>
</td>
</tr>
</table>

#### addr1v94725lv4umktv89cg2t04qjn4qq3p6l6zegvtx5esu2zuqfd487u

`[default]`

<table>
<tr>
<td>
<pre>
Total results:       8051
Total data:     99.00 MiB
<br/>
Slowest:        0.1508 s
Fastest:        0.1002 s
Average:        0.1349 s
<br/>
50% within:     0.1431 s
95% within:     0.1484 s
</pre>
</td>
<td>
<pre>
0.100 [1]  |■■■
0.105 [2]  |■■■■■■
0.110 [3]  |■■■■■■■■■
0.115 [1]  |■■■
0.120 [0]  |
0.125 [0]  |
0.131 [1]  |■■■
0.136 [0]  |
0.141 [4]  |■■■■■■■■■■■■
0.146 [8]  |■■■■■■■■■■■■■■■■■■■■■■■■■
0.151 [10] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
</pre>
</td>
</tr>
</table>

`?created_after=98677654&created_before=98764054`

<table>
<tr>
<td>
<pre>
Total results:       156
Total data:     1.83 MiB
<br/>
Slowest:        0.1374 s
Fastest:        0.0208 s
Average:        0.0616 s
<br/>
50% within:     0.0475 s
95% within:     0.1370 s
</pre>
</td>
<td>
<pre>
0.021 [1]  |■
0.032 [0]  |
0.044 [3]  |■■■■
0.056 [20] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
0.067 [1]  |■
0.079 [0]  |
0.091 [0]  |
0.102 [0]  |
0.114 [0]  |
0.126 [0]  |
0.137 [5]  |■■■■■■■■
</pre>
</td>
</tr>
</table>


#### \*@4301551ce28e83ef1082432f57a13bbbd389f4628592b73d71ca19e8833c0eb7

`[default]`

<table>
<tr>
<td>
<pre>
Total results:         1
Total data:     11.8 KiB
<br/>
Slowest:        0.0019 s
Fastest:        0.0003 s
Average:        0.0007 s
<br/>
50% within:     0.0006 s
95% within:     0.0017 s
</pre>
</td>
<td>
<pre>
0.000 [1] |■■■
0.000 [5] |■■■■■■■■■■■■■■■■■
0.001 [9] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
0.001 [4] |■■■■■■■■■■■■■■
0.001 [4] |■■■■■■■■■■■■■■
0.001 [5] |■■■■■■■■■■■■■■■■■
0.001 [0] |
0.001 [0] |
0.002 [0] |
0.002 [1] |■■■
0.002 [1] |■■■
</pre>
</td>
</tr>
</table>


## Comparative analysis

It is possible to compare two versions of Kupo. This is used by the developers to compare a future version with the last version.

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

## Last two versions

Here are the results of a comparative analysis of Kupo 2.11.0 (control) and Kupo 2.12.0 (experimental). The experiment was done twice.

First time:
```
Comparing:
data/20260722-070332 (s1: experimental group)
data/20260722-065951 (s2: control group)
Common datasets:
1 2 3 4 5 6 7 8
Significance level: α = 0.05
1: errors (s1/s2): (0/0); comparison: not significant
2: errors (s1/s2): (0/0); comparison: not significant
3: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0916194901341525
4: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0535370652888765
5: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.1398644489218612
6: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.5449497464477424
7: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.1963714961632728
8: errors (s1/s2): (0/0); comparison: s1 faster by factor 4.8343761442035224e-4
```

Second time:
```
Comparing:
data/20260722-071609 (s1: experimental group)
data/20260722-071055 (s2: control group)
Common datasets:
1 2 3 4 5 6 7 8
Significance level: α = 0.05
1: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.100916485402807
2: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0708009466150419
3: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.0582589486699308
4: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.058555515473119
5: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.084723814838676
6: errors (s1/s2): (0/0); comparison: s1 slower by factor 1.2807685516569087
7: errors (s1/s2): (0/0); comparison: not significant
8: errors (s1/s2): (0/0); comparison: s1 faster by factor 4.539446387978876e-4
```

This shows fairly consistently that in general, the new version is very slightly slower, but specifically in the case of dataset 8 the new version is blazingly faster (cf. [Issue 194][issue194]).


[oha]: https://github.com/hatoo/oha
[tap]: https://github.com/notunrandom/homebrew-cardano
[issue194]: https://github.com/CardanoSolutions/kupo/issues/194
