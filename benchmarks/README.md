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

TODO - wrap in HTML tables as above?

```
Preprod benchmarks
http://127.0.0.1:1442/matches/stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r
Summary:
  Success rate:	100.00%
  Total:	40.3126 ms
  Slowest:	33.0621 ms
  Fastest:	0.7050 ms
  Average:	6.2954 ms
  Requests/sec:	744.1845

  Total data:	98.94 KiB
  Size/request:	3.30 KiB
  Size/sec:	2.40 MiB

Response time histogram:
   0.705 ms [1]  |■
   3.941 ms [21] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   7.176 ms [0]  |
  10.412 ms [0]  |
  13.648 ms [5]  |■■■■■■■
  16.884 ms [0]  |
  20.119 ms [0]  |
  23.355 ms [0]  |
  26.591 ms [0]  |
  29.826 ms [0]  |
  33.062 ms [3]  |■■■■

Response time distribution:
  10.00% in 1.5734 ms
  25.00% in 1.6346 ms
  50.00% in 1.9557 ms
  75.00% in 10.7896 ms
  90.00% in 31.4457 ms
  95.00% in 31.7721 ms
  99.00% in 33.0621 ms
  99.90% in 33.0621 ms
  99.99% in 33.0621 ms


Details (average, fastest, slowest):
  DNS+dialup:	1.9311 ms, 1.6715 ms, 2.0235 ms
  DNS-lookup:	0.5915 ms, 0.0025 ms, 0.8418 ms

Status code distribution:
  [200] 30 responses

Total results
 6


http://127.0.0.1:1442/matches/stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r?spent_after=98245654
Summary:
  Success rate:	100.00%
  Total:	9.4558 ms
  Slowest:	6.0993 ms
  Fastest:	0.9329 ms
  Average:	2.0407 ms
  Requests/sec:	3172.6586

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	6.20 KiB

Response time histogram:
  0.933 ms [1]  |■■■
  1.450 ms [10] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  1.966 ms [6]  |■■■■■■■■■■■■■■■■■■■
  2.483 ms [7]  |■■■■■■■■■■■■■■■■■■■■■■
  2.999 ms [2]  |■■■■■■
  3.516 ms [2]  |■■■■■■
  4.033 ms [0]  |
  4.549 ms [1]  |■■■
  5.066 ms [0]  |
  5.583 ms [0]  |
  6.099 ms [1]  |■■■

Response time distribution:
  10.00% in 1.1577 ms
  25.00% in 1.3052 ms
  50.00% in 1.8326 ms
  75.00% in 2.3518 ms
  90.00% in 3.2082 ms
  95.00% in 4.2998 ms
  99.00% in 6.0993 ms
  99.90% in 6.0993 ms
  99.99% in 6.0993 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.3826 ms, 0.2055 ms, 0.5860 ms
  DNS-lookup:	0.0664 ms, 0.0016 ms, 0.2757 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw
Summary:
  Success rate:	100.00%
  Total:	6.2276 sec
  Slowest:	6.2267 sec
  Fastest:	0.1799 sec
  Average:	1.4164 sec
  Requests/sec:	4.8173

  Total data:	247.00 MiB
  Size/request:	8.23 MiB
  Size/sec:	39.66 MiB

Response time histogram:
  0.180 sec [1]  |■
  0.785 sec [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  1.389 sec [0]  |
  1.994 sec [0]  |
  2.599 sec [0]  |
  3.203 sec [0]  |
  3.808 sec [0]  |
  4.413 sec [0]  |
  5.017 sec [0]  |
  5.622 sec [0]  |
  6.227 sec [5]  |■■■■■■

Response time distribution:
  10.00% in 0.1817 sec
  25.00% in 0.5078 sec
  50.00% in 0.5585 sec
  75.00% in 0.6160 sec
  90.00% in 6.1526 sec
  95.00% in 6.1675 sec
  99.00% in 6.2267 sec
  99.90% in 6.2267 sec
  99.99% in 6.2267 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0004 sec, 0.0002 sec, 0.0005 sec
  DNS-lookup:	0.0000 sec, 0.0000 sec, 0.0001 sec

Status code distribution:
  [503] 25 responses
  [200] 5 responses

Total results
 87515


http://127.0.0.1:1442/matches/addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw?created_after=98677654&created_before=98764054
Summary:
  Success rate:	100.00%
  Total:	286.9277 ms
  Slowest:	182.7742 ms
  Fastest:	29.1512 ms
  Average:	59.9756 ms
  Requests/sec:	104.5560

  Total data:	1.28 KiB
  Size/request:	43 B
  Size/sec:	4.45 KiB

Response time histogram:
   29.151 ms [1]  |■
   44.514 ms [18] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   59.876 ms [3]  |■■■■■
   75.238 ms [0]  |
   90.600 ms [4]  |■■■■■■■
  105.963 ms [1]  |■
  121.325 ms [0]  |
  136.687 ms [0]  |
  152.050 ms [0]  |
  167.412 ms [0]  |
  182.774 ms [3]  |■■■■■

Response time distribution:
  10.00% in 32.4562 ms
  25.00% in 34.8832 ms
  50.00% in 37.6838 ms
  75.00% in 75.9480 ms
  90.00% in 181.7753 ms
  95.00% in 182.3838 ms
  99.00% in 182.7742 ms
  99.90% in 182.7742 ms
  99.99% in 182.7742 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.5706 ms, 0.3535 ms, 0.9283 ms
  DNS-lookup:	0.1763 ms, 0.0029 ms, 0.4009 ms

Status code distribution:
  [200] 27 responses
  [503] 3 responses

Total results
 0


http://127.0.0.1:1442/matches/*@bc40cc86ed43d84d3367a7ff2f4a401dbaed885af96edf1c8fd7379402735699
Summary:
  Success rate:	100.00%
  Total:	9.7147 ms
  Slowest:	4.1286 ms
  Fastest:	0.9951 ms
  Average:	1.9722 ms
  Requests/sec:	3088.1011

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	6.03 KiB

Response time histogram:
  0.995 ms [1] |■■■■
  1.308 ms [4] |■■■■■■■■■■■■■■■■■■
  1.622 ms [6] |■■■■■■■■■■■■■■■■■■■■■■■■■■■
  1.935 ms [7] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  2.249 ms [3] |■■■■■■■■■■■■■
  2.562 ms [3] |■■■■■■■■■■■■■
  2.875 ms [4] |■■■■■■■■■■■■■■■■■■
  3.189 ms [0] |
  3.502 ms [0] |
  3.815 ms [1] |■■■■
  4.129 ms [1] |■■■■

Response time distribution:
  10.00% in 1.2322 ms
  25.00% in 1.5083 ms
  50.00% in 1.8208 ms
  75.00% in 2.3690 ms
  90.00% in 2.8238 ms
  95.00% in 3.5527 ms
  99.00% in 4.1286 ms
  99.90% in 4.1286 ms
  99.99% in 4.1286 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.4791 ms, 0.1480 ms, 0.8369 ms
  DNS-lookup:	0.0737 ms, 0.0023 ms, 0.1813 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/*?spent_after=98245654&spent_before=98245660
Summary:
  Success rate:	100.00%
  Total:	3.5357 10 sec
  Slowest:	3.5355 10 sec
  Fastest:	0.0178 10 sec
  Average:	0.6042 10 sec
  Requests/sec:	0.8485

  Total data:	59.66 KiB
  Size/request:	1.99 KiB
  Size/sec:	1.69 KiB

Response time histogram:
  0.018 10 sec [1]  |■
  0.370 10 sec [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.721 10 sec [0]  |
  1.073 10 sec [0]  |
  1.425 10 sec [0]  |
  1.777 10 sec [0]  |
  2.128 10 sec [0]  |
  2.480 10 sec [0]  |
  2.832 10 sec [0]  |
  3.184 10 sec [0]  |
  3.535 10 sec [5]  |■■■■■■

Response time distribution:
  10.00% in 0.0179 10 sec
  25.00% in 0.0179 10 sec
  50.00% in 0.0179 10 sec
  75.00% in 0.0181 10 sec
  90.00% in 3.5355 10 sec
  95.00% in 3.5355 10 sec
  99.00% in 3.5355 10 sec
  99.90% in 3.5355 10 sec
  99.99% in 3.5355 10 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0001 10 sec, 0.0000 10 sec, 0.0002 10 sec
  DNS-lookup:	0.0000 10 sec, 0.0000 10 sec, 0.0000 10 sec

Status code distribution:
  [503] 25 responses
  [200] 5 responses

Total results
 15

```
