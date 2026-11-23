# Kupo mainnet benchmarks

## Parameters

* Date: 2026-08-20
* Version: 2.12.0
* Hardware: VM with 4 cores and 24G RAM
* Network: mainnet at slot 195643770
* SQLite dataset:
```
sqlite> SELECT COUNT(*) FROM inputs;
╭──────────╮
│ COUNT(*) │
╞══════════╡
│ 11171749 │
╰──────────╯
sqlite> SELECT COUNT(*) FROM policies;
╭──────────╮
│ COUNT(*) │
╞══════════╡
│ 11207267 │
╰──────────╯
sqlite> SELECT COUNT(*) FROM binary_data;
╭──────────╮
│ COUNT(*) │
╞══════════╡
│   797908 │
╰──────────╯
```

## Results

```
http://127.0.0.1:1442/matches/stake1uxqh9rn76n8nynsnyvf4ulndjv0srcc8jtvumut3989cqmgjt49h6
Summary:
  Success rate:	100.00%
  Total:	2.1629 10 sec
  Slowest:	2.1627 10 sec
  Fastest:	0.0176 10 sec
  Average:	0.3690 10 sec
  Requests/sec:	1.3870

  Total data:	531.49 MiB
  Size/request:	17.72 MiB
  Size/sec:	24.57 MiB

Response time histogram:
  0.018 10 sec [1]  |■
  0.232 10 sec [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.447 10 sec [0]  |
  0.661 10 sec [0]  |
  0.876 10 sec [0]  |
  1.090 10 sec [0]  |
  1.305 10 sec [0]  |
  1.519 10 sec [0]  |
  1.734 10 sec [0]  |
  1.948 10 sec [0]  |
  2.163 10 sec [5]  |■■■■■■

Response time distribution:
  10.00% in 0.0176 10 sec
  25.00% in 0.0176 10 sec
  50.00% in 0.0177 10 sec
  75.00% in 0.0185 10 sec
  90.00% in 2.1166 10 sec
  95.00% in 2.1396 10 sec
  99.00% in 2.1627 10 sec
  99.90% in 2.1627 10 sec
  99.99% in 2.1627 10 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0001 10 sec, 0.0001 10 sec, 0.0001 10 sec
  DNS-lookup:	0.0000 10 sec, 0.0000 10 sec, 0.0000 10 sec

Status code distribution:
  [503] 25 responses
  [200] 5 responses

Total results
 176383


http://127.0.0.1:1442/matches/stake1uxqh9rn76n8nynsnyvf4ulndjv0srcc8jtvumut3989cqmgjt49h6?spent_after=98245654
Summary:
  Success rate:	100.00%
  Total:	1352.2159 ms
  Slowest:	629.9416 ms
  Fastest:	177.7128 ms
  Average:	311.1133 ms
  Requests/sec:	22.1858

  Total data:	18.33 KiB
  Size/request:	625 B
  Size/sec:	13.55 KiB

Response time histogram:
  177.713 ms [1]  |■■
  222.936 ms [14] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  268.159 ms [0]  |
  313.381 ms [0]  |
  358.604 ms [2]  |■■■■
  403.827 ms [3]  |■■■■■■
  449.050 ms [6]  |■■■■■■■■■■■■■
  494.273 ms [2]  |■■■■
  539.496 ms [0]  |
  584.719 ms [0]  |
  629.942 ms [2]  |■■■■

Response time distribution:
  10.00% in 179.2388 ms
  25.00% in 181.9347 ms
  50.00% in 316.0035 ms
  75.00% in 427.2687 ms
  90.00% in 468.5867 ms
  95.00% in 629.6032 ms
  99.00% in 629.9416 ms
  99.90% in 629.9416 ms
  99.99% in 629.9416 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.1632 ms, 0.0660 ms, 0.2846 ms
  DNS-lookup:	0.0165 ms, 0.0028 ms, 0.0494 ms

Status code distribution:
  [200] 15 responses
  [503] 15 responses

Total results
 1


http://127.0.0.1:1442/matches/1d7f33bd23d85e1a25d87d86fac4f199c3197a2f7afeb662a0f34e1e.*
Summary:
  Success rate:	100.00%
  Total:	2.5634 10 sec
  Slowest:	2.5634 10 sec
  Fastest:	0.0176 10 sec
  Average:	0.4397 10 sec
  Requests/sec:	1.1703

  Total data:	311.78 MiB
  Size/request:	10.39 MiB
  Size/sec:	12.16 MiB

Response time histogram:
  0.018 10 sec [1]  |■
  0.272 10 sec [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.527 10 sec [0]  |
  0.781 10 sec [0]  |
  1.036 10 sec [0]  |
  1.290 10 sec [0]  |
  1.545 10 sec [0]  |
  1.800 10 sec [0]  |
  2.054 10 sec [0]  |
  2.309 10 sec [0]  |
  2.563 10 sec [5]  |■■■■■■

Response time distribution:
  10.00% in 0.0176 10 sec
  25.00% in 0.0176 10 sec
  50.00% in 0.0177 10 sec
  75.00% in 0.0179 10 sec
  90.00% in 2.5526 10 sec
  95.00% in 2.5582 10 sec
  99.00% in 2.5634 10 sec
  99.90% in 2.5634 10 sec
  99.99% in 2.5634 10 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0000 10 sec, 0.0000 10 sec, 0.0000 10 sec
  DNS-lookup:	0.0000 10 sec, 0.0000 10 sec, 0.0000 10 sec

Status code distribution:
  [503] 25 responses
  [200] 5 responses

Total results
 48923


http://127.0.0.1:1442/matches/1d7f33bd23d85e1a25d87d86fac4f199c3197a2f7afeb662a0f34e1e.*?created_before=98245654&spent_after=98764054
Summary:
  Success rate:	100.00%
  Total:	1667.3463 ms
  Slowest:	963.4973 ms
  Fastest:	176.5719 ms
  Average:	382.0051 ms
  Requests/sec:	17.9927

  Total data:	23.12 KiB
  Size/request:	789 B
  Size/sec:	13.87 KiB

Response time histogram:
  176.572 ms [1]  |■
  255.264 ms [19] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  333.957 ms [0]  |
  412.649 ms [0]  |
  491.342 ms [0]  |
  570.035 ms [1]  |■
  648.727 ms [1]  |■
  727.420 ms [1]  |■
  806.112 ms [2]  |■■■
  884.805 ms [2]  |■■■
  963.497 ms [3]  |■■■■■

Response time distribution:
  10.00% in 178.1230 ms
  25.00% in 179.9686 ms
  50.00% in 182.3605 ms
  75.00% in 693.5092 ms
  90.00% in 910.1717 ms
  95.00% in 915.7735 ms
  99.00% in 963.4973 ms
  99.90% in 963.4973 ms
  99.99% in 963.4973 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.1091 ms, 0.0499 ms, 0.2106 ms
  DNS-lookup:	0.0106 ms, 0.0025 ms, 0.0232 ms

Status code distribution:
  [503] 20 responses
  [200] 10 responses

Total results
 2


http://127.0.0.1:1442/matches/addr1v94725lv4umktv89cg2t04qjn4qq3p6l6zegvtx5esu2zuqfd487u
Summary:
  Success rate:	100.00%
  Total:	29.1220 ms
  Slowest:	28.1801 ms
  Fastest:	3.3902 ms
  Average:	6.2361 ms
  Requests/sec:	1030.1504

  Total data:	298.36 KiB
  Size/request:	9.95 KiB
  Size/sec:	10.01 MiB

Response time histogram:
   3.390 ms [1]  |■
   5.869 ms [18] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   8.348 ms [10] |■■■■■■■■■■■■■■■■■
  10.827 ms [0]  |
  13.306 ms [0]  |
  15.785 ms [0]  |
  18.264 ms [0]  |
  20.743 ms [0]  |
  23.222 ms [0]  |
  25.701 ms [0]  |
  28.180 ms [1]  |■

Response time distribution:
  10.00% in 4.1614 ms
  25.00% in 4.5325 ms
  50.00% in 5.3030 ms
  75.00% in 6.4684 ms
  90.00% in 8.0275 ms
  95.00% in 8.1721 ms
  99.00% in 28.1801 ms
  99.90% in 28.1801 ms
  99.99% in 28.1801 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.1021 ms, 0.0450 ms, 0.1906 ms
  DNS-lookup:	0.0112 ms, 0.0019 ms, 0.0278 ms

Status code distribution:
  [200] 30 responses

Total results
 25


http://127.0.0.1:1442/matches/addr1v94725lv4umktv89cg2t04qjn4qq3p6l6zegvtx5esu2zuqfd487u?created_after=98677654&created_before=98764054
Summary:
  Success rate:	100.00%
  Total:	30.6304 ms
  Slowest:	27.6520 ms
  Fastest:	0.8582 ms
  Average:	4.1921 ms
  Requests/sec:	979.4186

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	1.91 KiB

Response time histogram:
   0.858 ms [1]  |■
   3.538 ms [26] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   6.217 ms [0]  |
   8.896 ms [0]  |
  11.576 ms [0]  |
  14.255 ms [0]  |
  16.934 ms [0]  |
  19.614 ms [0]  |
  22.293 ms [0]  |
  24.973 ms [0]  |
  27.652 ms [3]  |■■■

Response time distribution:
  10.00% in 1.0317 ms
  25.00% in 1.2705 ms
  50.00% in 1.7218 ms
  75.00% in 2.1514 ms
  90.00% in 27.0475 ms
  95.00% in 27.5343 ms
  99.00% in 27.6520 ms
  99.90% in 27.6520 ms
  99.99% in 27.6520 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.0887 ms, 0.0412 ms, 0.1493 ms
  DNS-lookup:	0.0125 ms, 0.0021 ms, 0.0326 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/*@4301551ce28e83ef1082432f57a13bbbd389f4628592b73d71ca19e8833c0eb7
Summary:
  Success rate:	100.00%
  Total:	33.6091 ms
  Slowest:	27.9180 ms
  Fastest:	0.9648 ms
  Average:	4.3417 ms
  Requests/sec:	892.6157

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	1.74 KiB

Response time histogram:
   0.965 ms [1]  |■
   3.660 ms [26] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   6.355 ms [0]  |
   9.051 ms [0]  |
  11.746 ms [0]  |
  14.441 ms [0]  |
  17.137 ms [0]  |
  19.832 ms [0]  |
  22.527 ms [0]  |
  25.223 ms [0]  |
  27.918 ms [3]  |■■■

Response time distribution:
  10.00% in 1.0897 ms
  25.00% in 1.3425 ms
  50.00% in 1.9378 ms
  75.00% in 2.3490 ms
  90.00% in 26.6906 ms
  95.00% in 27.4057 ms
  99.00% in 27.9180 ms
  99.90% in 27.9180 ms
  99.99% in 27.9180 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.1012 ms, 0.0429 ms, 0.1671 ms
  DNS-lookup:	0.0135 ms, 0.0019 ms, 0.0353 ms

Status code distribution:
  [200] 30 responses

Total results
 0
```
