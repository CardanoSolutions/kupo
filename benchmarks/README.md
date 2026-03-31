# Benchmarks (mainnet)

## Specifications

- Hardware: MacBook Pro (2021), Apple M1 Max (16-core), 32 GB
- Version: 2.8.0

## Parameters

Benchmarks are conducted on a local Kupo instance, using [`oha`](https://github.com/hatoo/oha) with the following parameters

- Concurrent clients: 8
- Total requests: 30

## Dataset

The data source used for the benchmarks is the pruned mainnet database matching on `*` from genesis until around slot = 115,816,544 (~ Feb 8th, 2024)
It contains a grand total of 11,406,779 indexed outputs, 91,641 unique token policies and 13,592,035 datums.

## Results

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

# Benchmarks (preprod)

## Specifications

- Hardware: MacBook Pro (2024), Apple M4, 16 GB RAM, in a UTM VM.
- Kupo Version: 2.11.0

## Parameters

Benchmarks are conducted on a local Kupo instance, using
[`oha`](https://github.com/hatoo/oha) with the following parameters

- Concurrent clients: 8
- Total requests: 30

## Dataset

The data source used for the benchmarks is the pruned preprod database matching
on `*` from genesis until around slot = 118,837,644 (~ Mar 26, 2026)

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

It contains a grand total of 18,990,368 indexed outputs, 155,668 unique
token policies and ??? datums.

The above statistics were obtained using the following SQL queries:

```sql
sqlite> SELECT COUNT(DISTINCT output_reference) FROM inputs;
18990368
sqlite> SELECT COUNT(DISTINCT policy_id) FROM policies;
155668
```

TODO: how to obtain number of datums?

## Results


```
Preprod benchmarks
http://127.0.0.1:1442/matches/stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r
Summary:
  Success rate:	100.00%
  Total:	39.8888 ms
  Slowest:	34.6126 ms
  Fastest:	0.9592 ms
  Average:	6.2998 ms
  Requests/sec:	752.0910

  Total data:	99.29 KiB
  Size/request:	3.31 KiB
  Size/sec:	2.43 MiB

Response time histogram:
   0.959 ms [1]  |■
   4.325 ms [21] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   7.690 ms [0]  |
  11.055 ms [5]  |■■■■■■■
  14.421 ms [0]  |
  17.786 ms [0]  |
  21.151 ms [0]  |
  24.517 ms [0]  |
  27.882 ms [0]  |
  31.247 ms [0]  |
  34.613 ms [3]  |■■■■

Response time distribution:
  10.00% in 1.2235 ms
  25.00% in 1.4874 ms
  50.00% in 1.8979 ms
  75.00% in 8.2840 ms
  90.00% in 33.0712 ms
  95.00% in 34.3138 ms
  99.00% in 34.6126 ms
  99.90% in 34.6126 ms
  99.99% in 34.6126 ms


Details (average, fastest, slowest):
  DNS+dialup:	1.6588 ms, 1.4473 ms, 1.7800 ms
  DNS-lookup:	0.4347 ms, 0.0020 ms, 0.6465 ms

Status code distribution:
  [200] 30 responses

Total results
 6


http://127.0.0.1:1442/matches/stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r?spent_after=98245654
Summary:
  Success rate:	100.00%
  Total:	9.3218 ms
  Slowest:	6.9830 ms
  Fastest:	0.6721 ms
  Average:	2.0703 ms
  Requests/sec:	3218.2508

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	6.29 KiB

Response time histogram:
  0.672 ms [1]  |■■
  1.303 ms [4]  |■■■■■■■■■
  1.934 ms [13] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  2.565 ms [5]  |■■■■■■■■■■■■
  3.196 ms [4]  |■■■■■■■■■
  3.828 ms [1]  |■■
  4.459 ms [0]  |
  5.090 ms [1]  |■■
  5.721 ms [0]  |
  6.352 ms [0]  |
  6.983 ms [1]  |■■

Response time distribution:
  10.00% in 1.1857 ms
  25.00% in 1.4059 ms
  50.00% in 1.7446 ms
  75.00% in 2.3155 ms
  90.00% in 3.7100 ms
  95.00% in 4.5280 ms
  99.00% in 6.9830 ms
  99.90% in 6.9830 ms
  99.99% in 6.9830 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.3885 ms, 0.2717 ms, 0.4756 ms
  DNS-lookup:	0.0304 ms, 0.0018 ms, 0.0961 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw
Summary:
  Success rate:	100.00%
  Total:	8.1558 sec
  Slowest:	8.1549 sec
  Fastest:	0.1789 sec
  Average:	1.4619 sec
  Requests/sec:	3.6783

  Total data:	244.42 MiB
  Size/request:	8.15 MiB
  Size/sec:	29.97 MiB

Response time histogram:
  0.179 sec [1]  |■
  0.977 sec [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  1.774 sec [0]  |
  2.572 sec [0]  |
  3.369 sec [0]  |
  4.167 sec [0]  |
  4.965 sec [0]  |
  5.762 sec [0]  |
  6.560 sec [0]  |
  7.357 sec [0]  |
  8.155 sec [5]  |■■■■■■

Response time distribution:
  10.00% in 0.1791 sec
  25.00% in 0.1792 sec
  50.00% in 0.1799 sec
  75.00% in 0.1808 sec
  90.00% in 7.9811 sec
  95.00% in 8.0210 sec
  99.00% in 8.1549 sec
  99.90% in 8.1549 sec
  99.99% in 8.1549 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0008 sec, 0.0002 sec, 0.0010 sec
  DNS-lookup:	0.0000 sec, 0.0000 sec, 0.0000 sec

Status code distribution:
  [503] 25 responses
  [200] 5 responses

Total results
 86342


http://127.0.0.1:1442/matches/addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw?created_after=98677654&created_before=98764054
Summary:
  Success rate:	100.00%
  Total:	212.7842 ms
  Slowest:	210.2276 ms
  Fastest:	29.6657 ms
  Average:	51.9895 ms
  Requests/sec:	140.9879

  Total data:	476 B
  Size/request:	15 B
  Size/sec:	2.18 KiB

Response time histogram:
   29.666 ms [1]  |■
   47.722 ms [26] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   65.778 ms [0]  |
   83.834 ms [0]  |
  101.890 ms [0]  |
  119.947 ms [0]  |
  138.003 ms [0]  |
  156.059 ms [0]  |
  174.115 ms [0]  |
  192.171 ms [1]  |■
  210.228 ms [2]  |■■

Response time distribution:
  10.00% in 32.4199 ms
  25.00% in 33.5551 ms
  50.00% in 35.6305 ms
  75.00% in 38.0882 ms
  90.00% in 182.1083 ms
  95.00% in 209.9236 ms
  99.00% in 210.2276 ms
  99.90% in 210.2276 ms
  99.99% in 210.2276 ms


Details (average, fastest, slowest):
  DNS+dialup:	1.1789 ms, 0.5751 ms, 1.7241 ms
  DNS-lookup:	0.2367 ms, 0.0033 ms, 0.5799 ms

Status code distribution:
  [200] 29 responses
  [503] 1 responses

Total results
 0


http://127.0.0.1:1442/matches/*@bc40cc86ed43d84d3367a7ff2f4a401dbaed885af96edf1c8fd7379402735699
Summary:
  Success rate:	100.00%
  Total:	35.6143 ms
  Slowest:	33.0405 ms
  Fastest:	0.7162 ms
  Average:	4.6884 ms
  Requests/sec:	842.3575

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	1.64 KiB

Response time histogram:
   0.716 ms [1]  |■
   3.949 ms [25] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   7.181 ms [1]  |■
  10.414 ms [0]  |
  13.646 ms [0]  |
  16.878 ms [0]  |
  20.111 ms [0]  |
  23.343 ms [0]  |
  26.576 ms [0]  |
  29.808 ms [0]  |
  33.040 ms [3]  |■■■

Response time distribution:
  10.00% in 0.8104 ms
  25.00% in 1.0127 ms
  50.00% in 1.1727 ms
  75.00% in 3.1173 ms
  90.00% in 32.3182 ms
  95.00% in 32.8385 ms
  99.00% in 33.0405 ms
  99.90% in 33.0405 ms
  99.99% in 33.0405 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.6855 ms, 0.3095 ms, 1.3621 ms
  DNS-lookup:	0.0764 ms, 0.0019 ms, 0.1546 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/*?spent_after=98245654&spent_before=98245660
Summary:
  Success rate:	100.00%
  Total:	31.4874 ms
  Slowest:	29.5086 ms
  Fastest:	1.0373 ms
  Average:	7.3920 ms
  Requests/sec:	952.7628

  Total data:	297.89 KiB
  Size/request:	9.93 KiB
  Size/sec:	9.24 MiB

Response time histogram:
   1.037 ms [1]  |■■
   3.884 ms [16] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   6.732 ms [0]  |
   9.579 ms [8]  |■■■■■■■■■■■■■■■■
  12.426 ms [2]  |■■■■
  15.273 ms [0]  |
  18.120 ms [0]  |
  20.967 ms [0]  |
  23.814 ms [0]  |
  26.661 ms [0]  |
  29.509 ms [3]  |■■■■■■

Response time distribution:
  10.00% in 1.9015 ms
  25.00% in 2.5448 ms
  50.00% in 3.6228 ms
  75.00% in 8.8381 ms
  90.00% in 28.8317 ms
  95.00% in 29.3221 ms
  99.00% in 29.5086 ms
  99.90% in 29.5086 ms
  99.99% in 29.5086 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.4796 ms, 0.1296 ms, 0.8225 ms
  DNS-lookup:	0.0867 ms, 0.0017 ms, 0.2468 ms

Status code distribution:
  [200] 30 responses

Total results
 15
```
