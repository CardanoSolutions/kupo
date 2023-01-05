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

| options                              | duration  |
| ---                                  | ---       |
| `-`                                  | ?         |
| `--prune-utxo`, `--defer-db-indexes` | 13h 54min |

<p align="right">
  See also <a href="./synchronize.js">benchmarks/synchronize.js</a>
</p>

## Queries

Benchmarks are conducted on a local Kupo instance, using [ApacheBench (a.k.a `ab`)](https://en.wikipedia.org/wiki/ApacheBench) with the following parameters:

  - Concurrency level: 10
  - Total requests: 50

> **Note**
> The data source used for benchmarks is the pruned mainnet database matching on `*` from genesis
> until around slot = 81,361,362 (~ January 5th, 2023)
>
> It contains a grand total of 10,453,379 indexed outputs.

Results for a few handpicked patterns are collected and aggregated in the tables below.

#### By Address

| Query Flag                  | Results | Time Per Request (ms) |
| ---                         | ---     | ---                   |
| \-                          | 5489    | 122.959               |
| `?unspent`                  | 434     | 8.716                 |
| `?spent`                    | 5055    | 114.62                |
| `?created_after=(last 24h)` | 1434    | 52.235                |

#### By Payment Credentials


| Query Flag                  | Results | Time Per Request (ms) |
| ---                         | ---     | ---                   |
| \-                          | 5489    | 104.48                |
| `?unspent`                  | 434     | 8.347                 |
| `?spent`                    | 5055    | 86.999                |
| `?created_after=(last 24h)` | 1434    | 29.489                |

#### By Delegation Credentials

> **Note** The pattern used is [jpeg.store's](https://jpeg.store) contract stake credentials.

| Query Flag                  | Results | Time Per Request (ms) |
| ---                         | ---     | ---                   |
| \-                          | 273674  | 8886.613              |
| `?unspent`                  | 221976  | 5884.762              |
| `?spent`                    | 51695   | 1454.094              |
| `?created_after=(last 24h)` | 16302   | 2908.731              |

#### By Policy Id

> **Note** The pattern used is [$WMT's](https://worldmobiletoken.com/) token policy-id.

| Query Flag                  | Results | Time Per Request (ms) |
| ---                         | ---     | ---                   |
| \-                          | 49586   | 9389.623              |
| `?unspent`                  | 41883   | 6806.975              |
| `?spent`                    | 7704    | 5642.050              |
| `?created_after=(last 24h)` | 2251    | 4592.359              |

#### By Transaction Id

| Query Flag                  | Results | Time Per Request (ms) |
| ---                         | ---     | ---                   |
| \-                          | 1       | 3.127                 |
| `?unspent`                  | 0       | 1.689                 |
| `?spent`                    | 1       | 1.260                 |
| `?created_after=(last 24h)` | 0       | 2.062                 |

<p align="right">
  See also <a href="./queries.sh">benchmarks/queries.sh</a>
</p>
