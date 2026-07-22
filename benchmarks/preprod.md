# Kupo preprod benchmarks

## Parameters

* Date: 2026-07-22
* Version: 2.12.0
* Hardware: MacBook Pro (2024), Apple M4, Kupo and cardano-node in VM with 6 cores and 12G RAM
* Network: preprod at slot 129024056
* SQLite dataset:
```
sqlite> SELECT COUNT(*) FROM inputs;
╭──────────╮
│ COUNT(*) │
╞══════════╡
│  4285139 │
╰──────────╯
sqlite> SELECT COUNT(*) FROM policies;
╭──────────╮
│ COUNT(*) │
╞══════════╡
│  2025436 │
╰──────────╯
sqlite> SELECT COUNT(*) FROM binary_data;
╭──────────╮
│ COUNT(*) │
╞══════════╡
│  2914125 │
╰──────────╯
```

## Results

```
http://127.0.0.1:1442/matches/stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r
Summary:
  Success rate:	100.00%
  Total:	35.0280 ms
  Slowest:	34.2389 ms
  Fastest:	0.9850 ms
  Average:	5.8548 ms
  Requests/sec:	856.4587

  Total data:	40.14 KiB
  Size/request:	1.34 KiB
  Size/sec:	1.12 MiB

Response time histogram:
   0.985 ms [1]  |■
   4.310 ms [21] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   7.636 ms [2]  |■■■
  10.961 ms [3]  |■■■■
  14.287 ms [0]  |
  17.612 ms [0]  |
  20.937 ms [0]  |
  24.263 ms [0]  |
  27.588 ms [0]  |
  30.914 ms [0]  |
  34.239 ms [3]  |■■■■

Response time distribution:
  10.00% in 1.3626 ms
  25.00% in 1.5877 ms
  50.00% in 1.6696 ms
  75.00% in 7.2974 ms
  90.00% in 32.9061 ms
  95.00% in 34.0759 ms
  99.00% in 34.2389 ms
  99.90% in 34.2389 ms
  99.99% in 34.2389 ms


Details (average, fastest, slowest):
  DNS+dialup:	1.2139 ms, 0.4027 ms, 1.8513 ms
  DNS-lookup:	0.0519 ms, 0.0030 ms, 0.1490 ms

Status code distribution:
  [200] 30 responses

Total results
 3


http://127.0.0.1:1442/matches/stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r?spent_after=98245654
Summary:
  Success rate:	100.00%
  Total:	33.8104 ms
  Slowest:	32.6215 ms
  Fastest:	0.9239 ms
  Average:	5.1063 ms
  Requests/sec:	887.3015

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	1.73 KiB

Response time histogram:
   0.924 ms [1]  |■
   4.094 ms [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   7.263 ms [2]  |■■
  10.433 ms [0]  |
  13.603 ms [0]  |
  16.773 ms [0]  |
  19.942 ms [0]  |
  23.112 ms [0]  |
  26.282 ms [0]  |
  29.452 ms [1]  |■
  32.622 ms [2]  |■■

Response time distribution:
  10.00% in 1.2452 ms
  25.00% in 1.5062 ms
  50.00% in 2.2822 ms
  75.00% in 2.9347 ms
  90.00% in 29.3550 ms
  95.00% in 30.7791 ms
  99.00% in 32.6215 ms
  99.90% in 32.6215 ms
  99.99% in 32.6215 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.2988 ms, 0.1614 ms, 0.4678 ms
  DNS-lookup:	0.0120 ms, 0.0015 ms, 0.0507 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/a9fc2c980e6beed499b91089ca06ad433961a6238690219b8021fe43.*
Summary:
  Success rate:	100.00%
  Total:	3.2038 sec
  Slowest:	1.5532 sec
  Fastest:	0.1804 sec
  Average:	0.7470 sec
  Requests/sec:	9.3638

  Total data:	144.23 MiB
  Size/request:	4.81 MiB
  Size/sec:	45.02 MiB

Response time histogram:
  0.180 sec [1] |■■■■■
  0.318 sec [5] |■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.455 sec [6] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.592 sec [3] |■■■■■■■■■■■■■■■■
  0.729 sec [1] |■■■■■
  0.867 sec [0] |
  1.004 sec [4] |■■■■■■■■■■■■■■■■■■■■■
  1.141 sec [3] |■■■■■■■■■■■■■■■■
  1.279 sec [0] |
  1.416 sec [4] |■■■■■■■■■■■■■■■■■■■■■
  1.553 sec [3] |■■■■■■■■■■■■■■■■

Response time distribution:
  10.00% in 0.2021 sec
  25.00% in 0.3300 sec
  50.00% in 0.6366 sec
  75.00% in 1.1270 sec
  90.00% in 1.4292 sec
  95.00% in 1.4357 sec
  99.00% in 1.5532 sec
  99.90% in 1.5532 sec
  99.99% in 1.5532 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0005 sec, 0.0002 sec, 0.0009 sec
  DNS-lookup:	0.0000 sec, 0.0000 sec, 0.0001 sec

Status code distribution:
  [503] 16 responses
  [200] 14 responses

Total results
 7278


http://127.0.0.1:1442/matches/a9fc2c980e6beed499b91089ca06ad433961a6238690219b8021fe43.*?created_before=98245654&spent_after=98764054
Summary:
  Success rate:	100.00%
  Total:	439.6751 ms
  Slowest:	255.5334 ms
  Fastest:	63.0732 ms
  Average:	106.3578 ms
  Requests/sec:	68.2322

  Total data:	2.09 KiB
  Size/request:	71 B
  Size/sec:	4.75 KiB

Response time histogram:
   63.073 ms [1]  |■■
   82.319 ms [12] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  101.565 ms [6]  |■■■■■■■■■■■■■■■■
  120.811 ms [5]  |■■■■■■■■■■■■■
  140.057 ms [0]  |
  159.303 ms [0]  |
  178.549 ms [0]  |
  197.795 ms [5]  |■■■■■■■■■■■■■
  217.041 ms [0]  |
  236.287 ms [0]  |
  255.533 ms [1]  |■■

Response time distribution:
  10.00% in 63.7731 ms
  25.00% in 77.8454 ms
  50.00% in 85.2879 ms
  75.00% in 109.7108 ms
  90.00% in 183.5553 ms
  95.00% in 183.6082 ms
  99.00% in 255.5334 ms
  99.90% in 255.5334 ms
  99.99% in 255.5334 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.7236 ms, 0.0923 ms, 3.6836 ms
  DNS-lookup:	0.0513 ms, 0.0019 ms, 0.1934 ms

Status code distribution:
  [200] 25 responses
  [503] 5 responses

Total results
 0


http://127.0.0.1:1442/matches/addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw
Summary:
  Success rate:	100.00%
  Total:	3.2739 sec
  Slowest:	1.2784 sec
  Fastest:	0.2520 sec
  Average:	0.7904 sec
  Requests/sec:	9.1635

  Total data:	111.79 MiB
  Size/request:	3.73 MiB
  Size/sec:	34.14 MiB

Response time histogram:
  0.252 sec [1] |■■■■■
  0.355 sec [2] |■■■■■■■■■■
  0.457 sec [0] |
  0.560 sec [2] |■■■■■■■■■■
  0.663 sec [5] |■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.765 sec [4] |■■■■■■■■■■■■■■■■■■■■■
  0.868 sec [5] |■■■■■■■■■■■■■■■■■■■■■■■■■■
  0.970 sec [1] |■■■■■
  1.073 sec [6] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  1.176 sec [1] |■■■■■
  1.278 sec [3] |■■■■■■■■■■■■■■■■

Response time distribution:
  10.00% in 0.4831 sec
  25.00% in 0.6127 sec
  50.00% in 0.8241 sec
  75.00% in 1.0060 sec
  90.00% in 1.2101 sec
  95.00% in 1.2608 sec
  99.00% in 1.2784 sec
  99.90% in 1.2784 sec
  99.99% in 1.2784 sec


Details (average, fastest, slowest):
  DNS+dialup:	0.0004 sec, 0.0002 sec, 0.0007 sec
  DNS-lookup:	0.0000 sec, 0.0000 sec, 0.0001 sec

Status code distribution:
  [200] 22 responses
  [503] 8 responses

Total results
 12508


http://127.0.0.1:1442/matches/addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw?created_after=98677654&created_before=98764054
Summary:
  Success rate:	100.00%
  Total:	198.5826 ms
  Slowest:	198.0647 ms
  Fastest:	14.6098 ms
  Average:	37.6207 ms
  Requests/sec:	151.0706

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	302 B

Response time histogram:
   14.610 ms [1]  |■
   32.955 ms [26] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   51.301 ms [0]  |
   69.646 ms [0]  |
   87.992 ms [0]  |
  106.337 ms [0]  |
  124.683 ms [0]  |
  143.028 ms [0]  |
  161.374 ms [0]  |
  179.719 ms [0]  |
  198.065 ms [3]  |■■■

Response time distribution:
  10.00% in 17.5649 ms
  25.00% in 18.2926 ms
  50.00% in 19.7290 ms
  75.00% in 21.7135 ms
  90.00% in 197.6395 ms
  95.00% in 197.8989 ms
  99.00% in 198.0647 ms
  99.90% in 198.0647 ms
  99.99% in 198.0647 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.2909 ms, 0.1358 ms, 0.4638 ms
  DNS-lookup:	0.0129 ms, 0.0022 ms, 0.0335 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/*@bc40cc86ed43d84d3367a7ff2f4a401dbaed885af96edf1c8fd7379402735699
Summary:
  Success rate:	100.00%
  Total:	32.4974 ms
  Slowest:	28.7838 ms
  Fastest:	1.0973 ms
  Average:	4.8969 ms
  Requests/sec:	923.1501

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	1.80 KiB

Response time histogram:
   1.097 ms [1]  |■
   3.866 ms [23] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   6.635 ms [3]  |■■■■
   9.403 ms [0]  |
  12.172 ms [0]  |
  14.941 ms [0]  |
  17.709 ms [0]  |
  20.478 ms [0]  |
  23.247 ms [0]  |
  26.015 ms [0]  |
  28.784 ms [3]  |■■■■

Response time distribution:
  10.00% in 1.2584 ms
  25.00% in 1.4224 ms
  50.00% in 1.9372 ms
  75.00% in 3.2904 ms
  90.00% in 27.9656 ms
  95.00% in 28.6241 ms
  99.00% in 28.7838 ms
  99.90% in 28.7838 ms
  99.99% in 28.7838 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.6321 ms, 0.3371 ms, 0.8137 ms
  DNS-lookup:	0.0081 ms, 0.0014 ms, 0.0195 ms

Status code distribution:
  [200] 30 responses

Total results
 0


http://127.0.0.1:1442/matches/*?spent_after=98245654&spent_before=98245660
Summary:
  Success rate:	100.00%
  Total:	34.4610 ms
  Slowest:	28.3141 ms
  Fastest:	1.7580 ms
  Average:	4.5104 ms
  Requests/sec:	870.5493

  Total data:	60 B
  Size/request:	2 B
  Size/sec:	1.70 KiB

Response time histogram:
   1.758 ms [1]  |■
   4.414 ms [24] |■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
   7.069 ms [3]  |■■■■
   9.725 ms [0]  |
  12.380 ms [0]  |
  15.036 ms [0]  |
  17.692 ms [0]  |
  20.347 ms [0]  |
  23.003 ms [0]  |
  25.658 ms [0]  |
  28.314 ms [2]  |■■

Response time distribution:
  10.00% in 1.9880 ms
  25.00% in 2.0182 ms
  50.00% in 2.5250 ms
  75.00% in 3.3060 ms
  90.00% in 6.0600 ms
  95.00% in 27.6762 ms
  99.00% in 28.3141 ms
  99.90% in 28.3141 ms
  99.99% in 28.3141 ms


Details (average, fastest, slowest):
  DNS+dialup:	0.6229 ms, 0.1901 ms, 1.7914 ms
  DNS-lookup:	0.0165 ms, 0.0025 ms, 0.0550 ms

Status code distribution:
  [200] 30 responses

Total results
 0
```
