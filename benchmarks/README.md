# Benchmarks

- [Synchronization](#synchronization)
- [Queries](#queries)
  - [By Address](#by-address)
  - [By Payment Credentials](#by-payment-credentials)
  - [By Delegation credentials](#by-delegation-credentials)
  - [By Policy Id](#by-policy-id)
  - [By Transaction Id](#by-transaction-id)

## Specifications

- Hardware: MacBook Pro (2021), Apple M1 Max (16-core), 32 GB
- Version: 2.6.0

## Parameters

Benchmarks are conducted on a local Kupo instance, using [`oha`](https://github.com/hatoo/oha) with the following parameters

- Concurrent clients: 8
- Total requests: 30

## Dataset

The data source used for benchmarks is the pruned mainnet database matching on `*` from genesis until around slot = 98,555,773 (~ July 23rd, 2023)

It contains a grand total of 10,526,277 indexed outputs and 18,382,599 transactions with native tokens.

## Results

#### stake1uxqh9rn76n8nynsnyvf4ulndjv0srcc8jtvumut3989cqmgjt49h6

##### [default]

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

##### ?spent_after=98245654

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

##### [default]

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

##### ?created_before=98245654&spent_after=98764054

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

##### [default]

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

##### ?created_after=98677654&created_before=98764054

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

##### [default]

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
