# Benchmarks

> Exact numbers, especially time, on those benchmarks may not be of extreme relevance as they depend on the machine hardware specifications they were executed on. What is however interesting is the difference between versions and the measured impact of features additions to the base software. 

## Alonzo → Babbage Synchronization

| version  | options        | duration |
| ---      | ---            | ---      |
| `v1.0.1` | N/A            | 48min    |
| `v2.0.0` | `--prune-utxo` | 38min    |
| `v2.0.0` | \-             | 61min    |

<p align="right">
  See also <a href="./synchronize-alonzo.js">synchronize-alonzo.js</a>
</p>
