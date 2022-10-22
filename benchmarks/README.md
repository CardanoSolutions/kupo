# Benchmarks

- [Synchronization](#synchronization)
- [Queries](#queries)
  - [By Address](#by-address)
  - [By Payment Credentials](#by-payment-credentials)
  - [By Delegation credentials](#by-delegation-credentials)
  - [By Policy Id](#by-policy-id)
  - [By Transaction Id](#by-transaction-id)

## Hardware Specifications

- MacBook Pro (2021), Apple M1 Max (16-core), 32 GB

## Synchronization

### Mainnet

| version       | options        | duration             |
| ---           | ---            | ---                  |
| `v2.1.0`      | `--prune-utxo` | 11h 12min            |

<p align="right">
  See also <a href="./synchronize.js">benchmarks/synchronize.js</a>
</p>

## Queries

Benchmarks are conducted on a local Kupo instance, using [ApacheBench (a.k.a `ab`)](https://en.wikipedia.org/wiki/ApacheBench) with the following parameters:

  - Concurrency level: 10
  - Total requests: 50

Results for a few handpicked patterns are collected and aggregated in the tables below.

#### By Address

| Query Flag | Results | Time Per Request (ms) |
| ---        | ---     | ---                   |
| \-         | 2187    | 48.348                |
| `?unspent` | 465     | 9.1117                |
| `?spent`   | 1722    | 31.000                |

<details><summary>See details</summary>
  <details><summary>No query flag</summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        2187
  Total transferred:      53261750 bytes
  Requests per second:    206.83 [#/sec] (mean)
  Time per request:       48.348 [ms] (mean)
  Transfer rate:          215162.69 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.2      0       1
  Processing:    22   42  12.3     39      66
  Waiting:        3   13  11.2      6      37
  Total:         22   42  12.4     39      66

  Percentage of the requests served within a certain time (ms)
    50%     39
    66%     44
    75%     51
    80%     54
    90%     64
    95%     65
    98%     66
    99%     66
   100%     66 (longest request)
  ```
  </details>

  <details><summary><code>?unspent</code></summary>

  ```
  Concurrency Level:      10
  Time taken for tests:   0.046 seconds
  Complete requests:      50
  Failed requests:        0
  Matched results:        465
  Total transferred:      9867200 bytes
  Requests per second:    1096.88 [#/sec] (mean)
  Time per request:       9.117 [ms] (mean)
  Transfer rate:          211388.59 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.0      0       0
  Processing:     5    8   2.4      8      16
  Waiting:        1    2   1.4      2       9
  Total:          5    8   2.4      8      16

  Percentage of the requests served within a certain time (ms)
    50%      8
    66%      8
    75%      9
    80%     10
    90%     12
    95%     12
    98%     16
    99%     16
   100%     16 (longest request)
  ```
  </details>

  <details><summary><code>?spent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        1722
  Total transferred:      43402200 bytes
  Requests per second:    322.58 [#/sec] (mean)
  Time per request:       31.000 [ms] (mean)
  Transfer rate:          273454.89 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.0      0       0
  Processing:    22   30   3.4     30      42
  Waiting:        2    4   2.1      4      14
  Total:         22   30   3.4     30      42

  Percentage of the requests served within a certain time (ms)
    50%     30
    66%     30
    75%     31
    80%     33
    90%     34
    95%     35
    98%     42
    99%     42
   100%     42 (longest request)
  ```
  </details>
</details>

#### By Payment Credentials


| Query Flag | Results | Time Per Request (ms) |
| ---        | ---     | ---                   |
| \-         | 2187    | 39.204                |
| `?unspent` | 465     | 8.946                 |
| `?spent`   | 1722    | 30.688                |

<details><summary>See details</summary>
  <details><summary>No query flag</summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Results per requests:   2187
  Total transferred:      53261750 bytes
  Requests per second:    255.08 [#/sec] (mean)
  Time per request:       39.204 [ms] (mean)
  Transfer rate:          265347.56 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.0      0       0
  Processing:    29   37   3.7     37      47
  Waiting:        3    5   1.8      5      11
  Total:         29   37   3.7     37      47

  Percentage of the requests served within a certain time (ms)
    50%     37
    66%     39
    75%     40
    80%     40
    90%     41
    95%     42
    98%     47
    99%     47
   100%     47 (longest request)
  ```
  </details>

  <details><summary><code>?unspent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        465
  Total transferred:      9867200 bytes
  Requests per second:    1117.82 [#/sec] (mean)
  Time per request:       8.946 [ms] (mean)
  Transfer rate:          215424.49 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.0      0       0
  Processing:     5    8   2.0      8      14
  Waiting:        1    2   1.1      2       5
  Total:          5    8   2.0      8      14

  Percentage of the requests served within a certain time (ms)
    50%      8
    66%      9
    75%     10
    80%     10
    90%     11
    95%     11
    98%     14
    99%     14
   100%     14 (longest request)
  ```
  </details>

  <details><summary><code>?spent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        1722
  Total transferred:      43402200 bytes
  Requests per second:    325.86 [#/sec] (mean)
  Time per request:       30.688 [ms] (mean)
  Transfer rate:          276235.10 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.0      0       0
  Processing:    19   30   3.0     29      38
  Waiting:        2    4   1.7      4      11
  Total:         19   30   3.1     29      38

  Percentage of the requests served within a certain time (ms)
    50%     29
    66%     31
    75%     32
    80%     32
    90%     33
    95%     35
    98%     38
    99%     38
   100%     38 (longest request)
  ```
  </details>
</details>

#### By Delegation Credentials

> **Note** The pattern used is [jpeg.store's](https://jpeg.store) contract stake credentials.

| Query Flag | Results | Time Per Request (ms) |
| ---        | ---     | ---                   |
| \-         | 175987  | 4356.860              |
| `?unspent` | 147711  | 3269.528              |
| `?spent`   | 28276   | 740.410               |

<details><summary>See details</summary>
  <details><summary>No query flag</summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        175987
  Total transferred:      5373100196 bytes
  Requests per second:    2.30 [#/sec] (mean)
  Time per request:       4356.860 [ms] (mean)
  Transfer rate:          240869.26 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.2      0       1
  Processing:  2416 4078 834.4   3950    5419
  Waiting:      455 1165 624.1    898    2270
  Total:       2416 4078 834.5   3950    5419

  Percentage of the requests served within a certain time (ms)
    50%   3950
    66%   4416
    75%   4873
    80%   5016
    90%   5335
    95%   5409
    98%   5419
    99%   5419
   100%   5419 (longest request)
  ```
  </details>

  <details><summary><code>?unspent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        147711
  Total transferred:      4405634050 bytes
  Requests per second:    3.06 [#/sec] (mean)
  Time per request:       3269.528 [ms] (mean)
  Transfer rate:          263180.30 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.2      0       1
  Processing:  2215 3088 423.5   3138    3692
  Waiting:      294  665 233.2    653    1069
  Total:       2215 3088 423.6   3138    3692

  Percentage of the requests served within a certain time (ms)
    50%   3138
    66%   3306
    75%   3475
    80%   3533
    90%   3610
    95%   3670
    98%   3692
    99%   3692
   100%   3692 (longest request)
  ```
  </details>

  <details><summary><code>?spent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        28276
  Total transferred:      967516850 bytes
  Requests per second:    13.51 [#/sec] (mean)
  Time per request:       740.410 [ms] (mean)
  Transfer rate:          255221.08 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.1      0       0
  Processing:   362  687 108.2    690     946
  Waiting:       96  208  56.5    194     341
  Total:        362  687 108.2    690     946

  Percentage of the requests served within a certain time (ms)
    50%    690
    66%    742
    75%    749
    80%    774
    90%    824
    95%    844
    98%    946
    99%    946
   100%    946 (longest request)
  ```
  </details>
</details>

#### By Policy Id

> **Note** The pattern used is [$WMT's](https://worldmobiletoken.com/) token policy-id.

| Query Flag | Results | Time Per Request (ms) |
| ---        | ---     | ---                   |
| \-         | 40485   | 8627.671              |
| `?unspent` | 37737   | 3437.054              |
| `?spent`   | 2748    | 2503.479              |

<details><summary>See details</summary>
  <details><summary>No query flag</summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Non-2xx responses:      2
  Matched results:        40485
  Total transferred:      2522616472 bytes
  Requests per second:    1.16 [#/sec] (mean)
  Time per request:       8627.671 [ms] (mean)
  Transfer rate:          57106.78 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.2      0       1
  Processing:  1639 4081 6487.3   2603   35455
  Waiting:        0 1618 527.1   1570    2630
  Total:       1639 4081 6487.3   2604   35455

  Percentage of the requests served within a certain time (ms)
    50%   2604
    66%   3267
    75%   3328
    80%   3363
    90%   3707
    95%   3904
    98%  35455
    99%  35455
   100%  35455 (longest request)
  ```
  </details>

  <details><summary><code>?unspent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        37737
  Total transferred:      2105288100 bytes
  Requests per second:    2.91 [#/sec] (mean)
  Time per request:       3437.054 [ms] (mean)
  Transfer rate:          119634.16 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.1      0       1
  Processing:  1481 3226 394.3   3222    3753
  Waiting:      701 2247 358.0   2316    2815
  Total:       1481 3226 394.4   3222    3753

  Percentage of the requests served within a certain time (ms)
    50%   3222
    66%   3487
    75%   3560
    80%   3585
    90%   3647
    95%   3741
    98%   3753
    99%   3753
   100%   3753 (longest request)
  ```
  </details>

  <details><summary><code>?spent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        2748
  Total transferred:      522554200 bytes
  Requests per second:    3.99 [#/sec] (mean)
  Time per request:       2503.479 [ms] (mean)
  Transfer rate:          40767.82 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.3      0       2
  Processing:   895 2285 354.0   2331    3062
  Waiting:      734 2066 335.2   2101    2870
  Total:        895 2286 354.0   2331    3062

  Percentage of the requests served within a certain time (ms)
    50%   2331
    66%   2388
    75%   2564
    80%   2585
    90%   2654
    95%   2799
    98%   3062
    99%   3062
   100%   3062 (longest request)
  ```
  </details>
</details>

#### By Transaction Id

| Query Flag | Results | Time Per Request (ms) |
| ---        | ---     | ---                   |
| \-         | 2       | 3.280                 |
| `?unspent` | 0       | 1.179                 |
| `?spent`   | 2       | 1.098                 |

<details><summary>See details</summary>
  <details><summary>No query flag</summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        2
  Total transferred:      69600 bytes
  Requests per second:    3048.97 [#/sec] (mean)
  Time per request:       3.280 [ms] (mean)
  Transfer rate:          4144.69 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.1      0       0
  Processing:     0    2   2.9      1      13
  Waiting:        0    1   3.0      0      13
  Total:          0    2   2.9      1      13

  Percentage of the requests served within a certain time (ms)
    50%      1
    66%      1
    75%      1
    80%      1
    90%      2
    95%     13
    98%     13
    99%     13
   100%     13 (longest request)
  ```
  </details>

  <details><summary><code>?unspent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        0
  Total transferred:      7700 bytes
  Requests per second:    8478.89 [#/sec] (mean)
  Time per request:       1.179 [ms] (mean)
  Transfer rate:          1275.15 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.1      0       0
  Processing:     0    1   0.4      1       2
  Waiting:        0    1   0.3      1       1
  Total:          1    1   0.4      1       2

  Percentage of the requests served within a certain time (ms)
    50%      1
    66%      1
    75%      1
    80%      1
    90%      2
    95%      2
    98%      2
    99%      2
   100%      2 (longest request)
  ```
  </details>

  <details><summary><code>?spent</code></summary>

  ```
  Concurrency Level:      10
  Complete requests:      50
  Failed requests:        0
  Matched results:        2
  Total transferred:      69600 bytes
  Requests per second:    9107.47 [#/sec] (mean)
  Time per request:       1.098 [ms] (mean)
  Transfer rate:          12380.46 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   0.1      0       0
  Processing:     0    1   0.2      1       1
  Waiting:        0    0   0.1      0       1
  Total:          1    1   0.2      1       2

  Percentage of the requests served within a certain time (ms)
    50%      1
    66%      1
    75%      1
    80%      1
    90%      1
    95%      1
    98%      2
    99%      2
   100%      2 (longest request)
  ```
  </details>
</details>

<p align="right">
  See also <a href="./queries.sh">benchmarks/queries.sh</a>
</p>
