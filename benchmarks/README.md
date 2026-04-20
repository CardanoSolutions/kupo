# Kupo Benchmarks

This directory contains scripts that generate Kupo benchmarks.

- [bench](./bench) is a Bash script that generates benchmark results
- [stats.hs](./stats.hs) is a Haskell scripts that performs statistical
  comparisons of two benchmark data sets.

There are three options for running the `bench` script:

1. `./bench mainnet` benchmarks Kupo against [mainnet](#mainnet) and prints
   graphical results to the terminal.
2. `./bench preprod` benchmarks Kupo against [preprod](#preprod) and prints
   graphical results to the terminal.
3. `./bench data` benchmarks Kupo against preprod and writes CSV output to
   files for [comparative analysis](#comparative-analysis)

Current results of [mainnet](#mainnet) and [preprod](#preprod) benchmarks are
included below.

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

## Preprod

### Specifications

- Hardware: MacBook Pro (2024), Apple M4, 16 GB RAM, in a UTM VM.
- Kupo Version: 2.11.0

### Parameters

Benchmarks are conducted on a local Kupo instance, using
[`oha`](https://github.com/hatoo/oha) with the following parameters

- Concurrent clients: 8
- Total requests: 30

### Dataset

The data source used for the benchmarks is the pruned preprod database matching
on `*` from genesis until around slot = 119,440,417 (~ Apr 2, 2026)

Installed Kupo and ran preprod cardano-node using
<https://github.com/notunrandom/homebrew-cardano>.

Started Kupo with these arguments:

```bash
mac-vm:~ $ kupo \
> --node-socket $(brew --prefix)/var/cardano/preprod/node.socket \
> --node-config $(brew --prefix)/etc/cardano/preprod/config.json \
> --since origin --defer-db-indexes --prune-utxo --workdir ~/kupodb \
> --match "*"
```

It contains a grand total of 18,859,595 indexed outputs, 13,681,102 unique
token policies and 4,486,825 datums.

The above statistics were obtained using the following SQL queries:

```sql
sqlite> SELECT COUNT(*) FROM inputs;
18859595
sqlite> SELECT COUNT(*) FROM policies;
13681102
sqlite> SELECT COUNT(*) FROM binary_data;
4486825
```

### Results

```
Preprod benchmarks\n
http://127.0.0.1:1442/matches/stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r
Summary:
  Success rate:	100.00%
  Total:	93.3370 ms
  Slowest:	86.1881 ms
  Fastest:	0.5590 ms
  Average:	16.1459 ms
  Requests/sec:	321.4158

  Total data:	99.29 KiB
  Size/request:	3.31 KiB
  Size/sec:	1.04 MiB

Response time histogram:
   0.559 ms [1]  |■
   9.122 ms [21] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  17.685 ms [0]  |
  26.248 ms [0]  |
  34.811 ms [1]  |■
  43.374 ms [4]  |■■■■■■
  51.936 ms [0]  |
  60.499 ms [0]  |
  69.062 ms [0]  |
  77.625 ms [0]  |
  86.188 ms [3]  |■■■■

Response time distribution:
  10.00% in 1.4353 ms
  25.00% in 1.5288 ms
  50.00% in 1.7976 ms
  75.00% in 29.1718 ms
  90.00% in 84.9721 ms
  95.00% in 85.7780 ms
  99.00% in 86.1881 ms
  99.90% in 86.1881 ms
  99.99% in 86.1881 ms


Details (average, fastest, slowest):
  DNS+dialup:	1.6987 ms, 1.5678 ms, 1.9138 ms
  DNS-lookup:	0.5228 ms, 0.0017 ms, 0.7787 ms

Status code distribution:
  [200] 30 responses

Total results
 6


http://127.0.0.1:1442/matches/stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r?spent_after=98245654
Summary:
  Success rate:	100.00%
  Total:	9.8539 ms
  Slowest:	4.0949 ms
  Fastest:	0.4839 ms
  Average:	2.0406 ms
  Requests/sec:	3044.4876

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	5.95 KiB

Response time histogram:
  0.484 ms [1] |■■■■
  0.845 ms [0] |
  1.206 ms [0] |
  1.567 ms [7] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  1.928 ms [8] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  2.289 ms [4] |■■■■■■■■■■■■■■■■
  2.651 ms [5] |■■■■■■■■■■■■■■■■■■■■
  3.012 ms [1] |■■■■
  3.373 ms [3] |■■■■■■■■■■■■
  3.734 ms [0] |
  4.095 ms [1] |■■■■

Response time distribution:
  10.00% in 1.3371 ms
  25.00% in 1.5647 ms
  50.00% in 1.8597 ms
  75.00% in 2.5123 ms
  90.00% in 3.1562 ms
  95.00% in 3.2308 ms
  99.00% in 4.0949 ms
  99.90% in 4.0949 ms
  99.99% in 4.0949 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.3973 ms, 0.2376 ms, 0.5734 ms
  DNS-lookup:	0.1043 ms, 0.0023 ms, 0.2743 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/a9fc2c980e6beed499b91089ca06ad433961a6238690219b8021fe43.*
Summary:
  Success rate:	100.00%
  Total:	1.0966 10 sec
  Slowest:	1.0965 10 sec
  Fastest:	0.0179 10 sec
  Average:	0.2045 10 sec
  Requests/sec:	2.7358

  Total data:	450.29 MiB
  Size/request:	15.01 MiB
  Size/sec:	41.06 MiB

Response time histogram:
  0.018 10 sec [1]  |■
  0.126 10 sec [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.234 10 sec [0]  |
  0.341 10 sec [0]  |
  0.449 10 sec [0]  |
  0.557 10 sec [0]  |
  0.665 10 sec [0]  |
  0.773 10 sec [0]  |
  0.881 10 sec [0]  |
  0.989 10 sec [0]  |
  1.096 10 sec [5]  |■■■■■■

Response time distribution:
  10.00% in 0.0180 10 sec
  25.00% in 0.0182 10 sec
  50.00% in 0.0281 10 sec
  75.00% in 0.0573 10 sec
  90.00% in 1.0883 10 sec
  95.00% in 1.0959 10 sec
  99.00% in 1.0965 10 sec
  99.90% in 1.0965 10 sec
  99.99% in 1.0965 10 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0000 10 sec, 0.0000 10 sec, 0.0001 10 sec
  DNS-lookup:	0.0000 10 sec, 0.0000 10 sec, 0.0000 10 sec

Status code distribution:
  [503] 25 responses
  [200] 5 responses

Total results
 63058


http://127.0.0.1:1442/matches/a9fc2c980e6beed499b91089ca06ad433961a6238690219b8021fe43.*?created_before=98245654&spent_after=98764054
Summary:
  Success rate:	100.00%
  Total:	1476.0095 ms
  Slowest:	589.7672 ms
  Fastest:	179.6779 ms
  Average:	345.7223 ms
  Requests/sec:	20.3251

  Total data:	1.56 MiB
  Size/request:	53.13 KiB
  Size/sec:	1.05 MiB

Response time histogram:
  179.678 ms [1]  |■■
  220.687 ms [14] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  261.696 ms [0]  |
  302.705 ms [0]  |
  343.714 ms [0]  |
  384.723 ms [0]  |
  425.732 ms [1]  |■■
  466.740 ms [3]  |■■■■■■
  507.749 ms [4]  |■■■■■■■■■
  548.758 ms [2]  |■■■■
  589.767 ms [5]  |■■■■■■■■■■■

Response time distribution:
  10.00% in 180.2656 ms
  25.00% in 181.3761 ms
  50.00% in 418.4418 ms
  75.00% in 493.0484 ms
  90.00% in 580.2770 ms
  95.00% in 580.6327 ms
  99.00% in 589.7672 ms
  99.90% in 589.7672 ms
  99.99% in 589.7672 ms


Details (average, fastest, slowest):
  DNS+dialup:	1.0679 ms, 0.3551 ms, 1.4733 ms
  DNS-lookup:	0.2348 ms, 0.0027 ms, 0.5721 ms

Status code distribution:
  [200] 15 responses
  [503] 15 responses

Total results
 81


http://127.0.0.1:1442/matches/addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw
Summary:
  Success rate:	100.00%
  Total:	6.3064 sec
  Slowest:	6.3052 sec
  Fastest:	0.1800 sec
  Average:	1.4032 sec
  Requests/sec:	4.7570

  Total data:	244.45 MiB
  Size/request:	8.15 MiB
  Size/sec:	38.76 MiB

Response time histogram:
  0.180 sec [1]  |■
  0.793 sec [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  1.405 sec [0]  |
  2.018 sec [0]  |
  2.630 sec [0]  |
  3.243 sec [0]  |
  3.855 sec [0]  |
  4.468 sec [0]  |
  5.080 sec [0]  |
  5.693 sec [0]  |
  6.305 sec [5]  |■■■■■■

Response time distribution:
  10.00% in 0.1814 sec
  25.00% in 0.3892 sec
  50.00% in 0.5507 sec
  75.00% in 0.5693 sec
  90.00% in 6.2281 sec
  95.00% in 6.2421 sec
  99.00% in 6.3052 sec
  99.90% in 6.3052 sec
  99.99% in 6.3052 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0008 sec, 0.0003 sec, 0.0011 sec
  DNS-lookup:	0.0001 sec, 0.0000 sec, 0.0002 sec

Status code distribution:
  [503] 25 responses
  [200] 5 responses

Total results
 86332


http://127.0.0.1:1442/matches/addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw?created_after=98677654&created_before=98764054
Summary:
  Success rate:	100.00%
  Total:	294.6513 ms
  Slowest:	217.1138 ms
  Fastest:	32.5609 ms
  Average:	56.6616 ms
  Requests/sec:	101.8153

  Total data:	476 B
  Size/request:	15 B
  Size/sec:	1.58 KiB

Response time histogram:
   32.561 ms [1]  |■
   51.016 ms [25] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   69.471 ms [0]  |
   87.927 ms [0]  |
  106.382 ms [0]  |
  124.837 ms [1]  |■
  143.293 ms [0]  |
  161.748 ms [0]  |
  180.203 ms [0]  |
  198.659 ms [1]  |■
  217.114 ms [2]  |■■

Response time distribution:
  10.00% in 35.2467 ms
  25.00% in 35.7046 ms
  50.00% in 36.7210 ms
  75.00% in 39.6945 ms
  90.00% in 189.8290 ms
  95.00% in 209.2978 ms
  99.00% in 217.1138 ms
  99.90% in 217.1138 ms
  99.99% in 217.1138 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.3957 ms, 0.1520 ms, 0.7768 ms
  DNS-lookup:	0.0915 ms, 0.0022 ms, 0.1804 ms

Status code distribution:
  [200] 29 responses
  [503] 1 responses

Total results
 0


http://127.0.0.1:1442/matches/*@bc40cc86ed43d84d3367a7ff2f4a401dbaed885af96edf1c8fd7379402735699
Summary:
  Success rate:	100.00%
  Total:	35.1694 ms
  Slowest:	30.0879 ms
  Fastest:	0.5999 ms
  Average:	2.9454 ms
  Requests/sec:	853.0139

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	1.67 KiB

Response time histogram:
   0.600 ms [1]  |■
   3.549 ms [25] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   6.498 ms [3]  |■■■
   9.446 ms [0]  |
  12.395 ms [0]  |
  15.344 ms [0]  |
  18.293 ms [0]  |
  21.242 ms [0]  |
  24.190 ms [0]  |
  27.139 ms [0]  |
  30.088 ms [1]  |■

Response time distribution:
  10.00% in 0.9084 ms
  25.00% in 1.4329 ms
  50.00% in 1.8232 ms
  75.00% in 2.8876 ms
  90.00% in 4.0606 ms
  95.00% in 4.6730 ms
  99.00% in 30.0879 ms
  99.90% in 30.0879 ms
  99.99% in 30.0879 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.4867 ms, 0.3432 ms, 0.7280 ms
  DNS-lookup:	0.0316 ms, 0.0018 ms, 0.1201 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/*?spent_after=98245654&spent_before=98245660
Summary:
  Success rate:	100.00%
  Total:	19.0280 ms
  Slowest:	11.5676 ms
  Fastest:	2.3466 ms
  Average:	4.2811 ms
  Requests/sec:	1576.6274

  Total data:	297.89 KiB
  Size/request:	9.93 KiB
  Size/sec:	15.29 MiB

Response time histogram:
   2.347 ms [1]  |■■
   3.269 ms [13] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   4.191 ms [5]  |■■■■■■■■■■■■
   5.113 ms [4]  |■■■■■■■■■
   6.035 ms [2]  |■■■■
   6.957 ms [3]  |■■■■■■■
   7.879 ms [0]  |
   8.801 ms [0]  |
   9.723 ms [1]  |■■
  10.646 ms [0]  |
  11.568 ms [1]  |■■

Response time distribution:
  10.00% in 2.5770 ms
  25.00% in 2.8856 ms
  50.00% in 3.5824 ms
  75.00% in 5.0763 ms
  90.00% in 6.4232 ms
  95.00% in 9.3081 ms
  99.00% in 11.5676 ms
  99.90% in 11.5676 ms
  99.99% in 11.5676 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.3806 ms, 0.2884 ms, 0.5444 ms
  DNS-lookup:	0.0488 ms, 0.0027 ms, 0.1155 ms

Status code distribution:
  [200] 30 responses

Total results
 15
```

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
1. Wait for Kupo to finished rolling forward and create indexes.
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
1. Wait for Kupo to finished rolling forward and create indexes.
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
