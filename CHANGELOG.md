### [2.11.0] - UNRELEASED

#### Added

- New `--until=SLOT|POINT` command line option to instrument synchronization up to a certain point.

- New endpoint `/metrics`, which returns the server health. The semantic is slightly different from the `/health` endpoint in that it always return a status `200 OK` even when the server isn't connected or far from the tip.

- New health metric added:
  - `seconds_since_last_block`: number of seconds elapsed since the last block was ingested by Kupo;
  - `network_synchronization`: a progress percentage of the synchronization towards the node tip.

#### Changed

- Fix `hydra` integration, now supporting `0.20.0` and at least upcoming `0.21.0`. The API changed in version `0.20.0` such that transactions are fully contained.

#### Removed

N/A

### [2.10.0] - 2025-01-03

#### Added

> [!WARNING]
> This version requires an (automatic) database migration which will set columns values for existing rows to `NULL` for all inputs. To maintain backward compatibility, this is handled gracefully by the server which will also provide `null` JSON values for the relevant fields. However, this means that to get access to the newly introduced `transaction_id`, `input_index` and `redeemer` value for the `spent_at` fields, a re-sync since the beginning of the Alonzo era is necessary.

- Kupo now keeps track of the transaction id and input index that is spending a TxO, as well as the redeemer if any. These now optionally appear under the `spent_at` field for matches. The redeemer is provided as a base16-encoded CBOR Plutus Data, akin to how datums are already provided.

- Add `?resolve_hashes` query flag to GET matches queries

  When set, this will instrument the server to perform joins on datums
  and scripts to retrieve any known values associated to hashes. Scripts
  are returned in the same format as the GET scripts endpoint. Similarly
  for datums.

  Also, `?resolve_hashes` causes all match responses to always
  contain additional `datum` and `script` fields. Those fields can be
  `null` if:

  - The underlying output has no datum or script whatsoever.
  - The underlying datum or script isn't known at this point.

  The latter is only possible for datums, since they can be published as
  reference, and only revealed later on in a transaction witness.

#### Changed

- Integrate with `cardano-node==10.1.3`.

#### Removed

N/A

### [2.9.0] - 2024-07-19

#### Added

- `tip` is now supported as a value for the `--since` option. When provided, the indexing process will start wherever the blockchain source (node, ogmios, ...) is at.

#### Changed

- Integrate with `cardano-node==9.0.0`.

- Integrate with `hydra-node==0.16.0`.

#### Removed

N/A

### [2.8.0] - 2024-02-09

#### Added

- A new mode `--read-only` which can be used to boot-up an HTTP server with only read access to the underlying database. This option comes as an alternative to the other options for chain producers (e.g. `--node-socket` and `--node-config`). The replica can only reply successfully to GET queries with the exception of queries under `/metadata`. The latter must go through the master server.

- Automatic restart and setup indexes when `--defer-db-indexes` is provided and the tip of the chain is reached.

- A new field `configuration.indexes` to the health object that indicates whether the server is still deferring the installation of lookup indexes or has already installed them.

#### Changed

- Integrate with `cardano-node==8.7.2` including the (preliminary) Conway era.

- The server now returns:
  - `503` from `/health` if the `connection_status` is `"disconnected"`;
  - `202` if the server is `connected` but far away from the node's tip (i.e. still syncing)

- Reinstate WAL journal mode for SQLite main writer. This should allow setting up more concurrent readers on top of the same database.

- Fine-tune some internal constraints around database connections management for better performances. In particular, the maximum number of concurrent readers have been increased.

#### Remove

- 'Content-Length' header on some of the server responses which turned out to be already handled by the underlying server when necessary (according to the original HTTP specification).

### [2.7.2] - 2023-12-09

#### Added

- Add missing index on inputs' datum (speed up garbage collection).
- Automated aarch64 static builds for Linux.

#### Changed

- Fix missing runtime flags on pre-compiled binary.

#### Removed

- N/A

### [2.7.1] - 2023-12-03

#### Added

- N/A

#### Changed

- Upgraded internal dependencies to `cardano-node==8.6.0`.

- Fixed Kupo choking on mainnet pointer addresses following ledger internal updates in `cardano-node>=8.1.2`. See also: [input-output-hk/cardano-ledger#3898](https://github.com/input-output-hk/cardano-ledger/issues/3898).

- Fixed Kupo not indexing inline-scripts in output until they were used in a transaction. See also [#148](https://github.com/CardanoSolutions/kupo/issues/148).

- Binary data is now pruned incrementally to avoid blocking the main writer thread for too long, especially in scenario where the garbage collection has been 'snoozed' many times. See also [#143](https://github.com/CardanoSolutions/kupo/issues/143)

- Fixed wrong metadata hash being reported for metadata containing native or Plutus scripts.

#### Removed

- N/A

### [2.7.0] - 2023-10-13

#### Added

- Support for indexing a Hydra head.

  Use `--hydra-host` and `--hydra-port` to connect `kupo` to a `hydra-node`.

  > **Note**:
  >
  > Hydra heads do not actually form a 'chain'; Hydra doesn't have blocks, but
  > snapshots which are akin to blocks so we treat them as such. The block
  > header hash we opted for is arbitrary and is a hash of all the transaction
  > id present in the snapshot.
  >
  > It also comes with diminished capabilities since Hydra doesn't have any
  > protocol to query metadata for example and Kupo does not store them. So
  > unlike Ogmios or Cardano-node, metadata cannot be retrieved when Hydra is
  > used as a chain producer.

- Added TLS certificates to the Docker image so that remote connections to
  Ogmios or Hydra instances behind TLS can mostly work out-of-the-box

#### Changed

- N/A

#### Removed

- N/A

### [2.6.1] - 2023-08-30

#### Added

- N/A

#### Changed

- Fixed 'not enough bytes' exception sometimes raised when establishing a connection over TLS.
- Fixed connection failures for fetching metadata when the connection is configured over TLS.

#### Removed

- N/A

### [2.6.0] - 2023-08-16

#### Added

- Simple internal retrying strategy on HTTP request, this should make the experience with the API server smoother as request would fail less often. Transient failures would now likely be resolved internally.

#### Changed

- Support for (and require) Ogmios `>= v6.0.0` when using Ogmios as a blockchain provider.

- Use biggger cache and database page size. This makes queries 10-15% faster on average, at the expense of some extra memory usage during period of heavy loads. This new setting is a trial and may be made configurable if deemed necessary.

- Rollbacks are now done incrementally. This doesn't really affect normal
  operations as rollbacks are usually small already, but it does impact the
  dynamic addition of patterns with rollback points far in the past.

- Re-implemented a fix from v2.3.4 that got lost in translation regarding restart on failures.

- Fixed / reworked ANSI logs to be more compatible with fonts that do not support fancy chevron characters. Colors have also been adjusted for better readibility.

- Kupo now has a top-level panic hook acting as a 'catch all' exception. So unexpected failures will now be properly logged as `error` and cause the application to exit with an exit code 1.

#### Removed

- `--max-concurrency` command-line option is gone; Kupo now use sensible defaults based on the machine's detected capabilities.

### [2.5.1] - 2023-08-30

#### Added

- N/A

#### Changed

- Fixed 'not enough bytes' exception sometimes raised when establishing a connection over TLS. (backported from v2.6.1)
- Fixed connection failures for fetching metadata when the connection is configured over TLS. (backported from v2.6.1)
- Fixed / reworked ANSI logs to be more compatible with fonts that do not support fancy chevron characters. Colors have also been adjusted for better readibility. (backported from v2.6.0)
- Re-implemented a fix from v2.3.4 that got lost in translation regarding restart on failures.

#### Removed

- N/A

### [2.5.0] - 2023-07-22

#### Added

- Allow connection to remote Ogmios hosts behind TLS. This is now possible by prefixing the hostname with `wss://`.

- The HTTP server now replies to pre-flight requests (i.e. `OPTIONS`).

#### Changed

- The HTTP server is now accessible cross-origins by default (`Access-Control-Allow-Origin: *`).

- Increased logs surrounding rollbacks, and adjusted the management of internal
  indexes related to rollbacks. This should lead to a decrease of I/O activity
  during rollbacks, without noticeable impact on the application (provided that
  initial synchronization is done with `--defer-db-indexes`)

#### Removed

N/A

### [2.4.1] - 2023-06-29

#### Added

N/A

#### Changed

- Fix ANSI colors of logs in TTY terminals

- Fix internal dependency causing build issues for ouroboros-consensus

#### Removed

N/A

### [2.4.0] - 2023-02-23

#### Added

- [📌 #108](https://github.com/CardanoSolutions/kupo/issues/108) Introduce a new indexing pattern by transaction metadata tag. This pattern will only index outputs of a transaction that (a) has metadata, (b) has one metadatum labelled with the provided tag. This pattern is however (at least for now) only usable for indexing, not querying.

  → [📖 API Reference](https://cardanosolutions.github.io/kupo/#section/Patterns/Metadata-tag)

- Provide a new command `copy` to conveniently clone an existing database into a smaller subset. This is particularly useful to quickly fork new instances of a parent index without having to resynchronize the entire chain. The main use case being a scenario where one maintains a global index (i.e. matching `*`) and needs to create on-demand indexes using more restrictive patterns. The `copy` commands accepts one or many patterns and copies (even large) indexes in a matter of seconds.

  ```
  Usage: kupo copy --from DIR --into DIR [--match PATTERN]

    Copy from a source database into another, while applying the provided pattern
    filters.

  Available options:
    -h,--help                Show this help text
    --from DIR               Working directory to copy from.
    --into DIR               Working directory to copy into.
    --match PATTERN          A pattern to match on. Can be provided multiple times (as a logical disjunction, i.e. 'or')
  ```

- The `GET /health` endpoint now returns the version as part of the response.

#### Changed

- The server now implements some basic retry mechanism for some transient errors that can occur under heavy load (e.g. failing to open a database connection). This is mostly transparent for clients but should result in less `503` errors by providing a first retryable layer directly in the server.

- Add missing (optional) index on `policies` table to speed up queries by policy id and asset id.

- Executables from `master` will now return `nightly` as a version number, with a git commit hash. This is done in hope to reduce confusion when figuring out what version is a particular binary built against.

#### Removed

N/A

### [2.3.4] - 2023-01-26

#### Added

N/A

#### Changed

- Fixed a restart issue where Kupo will continue synchronize back from the checkpoint known at startup instead of the latest known checkpoint after loosing (and recovering) connection from its block provider (cardano-node or ogmios).

#### Removed

N/A

### [2.3.3] - 2023-01-23

#### Added

N/A

#### Changed

- Fixed a bug where Kupo would prune inputs at regular interval even if `--prune-utxo` isn't set. This only occurs when matching patterns are neither `*` nor `*/*`. Related, Kupo would also never prune binary data as it should; even with `--prune-utxo` enabled.

#### Removed

N/A

### [2.3.2] - 2023-01-18

#### Added

N/A

#### Changed

- Bumped version of the embedded SQLite stack to v3.40.1 and changed the main writer process journaling mode to `TRUNCATE`. These two changes come as a mitigation of an SQLite bug that resulted in sometimes throwing:

  > Error while attempting to perform step: cannot rollback - no transaction is active.

  The root cause for this error is hard to identify and may be due to a full disk space; It's been also linked to the write-ahead logging journal mode; which Kupo benefits only a little and only during syncing.

#### Removed

N/A


### [2.3.1] - 2023-01-09

#### Added

N/A

#### Changed

- [📌 #101](https://github.com/CardanoSolutions/kupo/issues/101) Starting from the Babbage era, transactions that fail phase-2 validations (i.e. script execution) can use a _collateral return_ output where to send change from a lost collateral. This makes collateral management easier for client applications and result in a special kind of UTxO in the ledger. Kupo has been wrongly attributing an index of `0` to those outputs, whereas the ledger indicates the following:

  > Note that the new $collOuts$ function generates a single output with an index $|txOuts_{txb}|$.

  This is now fixed. Database patches are provided for preview, preprod with the release notes of `v2.3.1` to update already indexed collateral returns if any.

  > **Note**
  >
  > There's no patch for mainnet because, to this date, there hasn't been any phase-2 failure on mainnet making use of collateral returns.

- Make internal db garbage-collection more incremental. This has two main benefits:

  1. It allows the consumer to preempt the database connection for writing blocks while a GC is happening. This is crucial to not start lagging behind because of a long-running (several minutes) garbage collection.
  2. It breaks the inputs pruning over multiple smaller transaction, which are much faster to process than a single (sometimes excessively large) transaction by SQLite under the hood; considerably reducing the garbage-collection delays.

- Passing `--defer-db-indexes` now also removes query indexes if present.

  Prior to this commit, passing --defer-db-indexes on a database with already present indexes would only raise a warning and there was no practical way to remove indexes from the database (other than doing it manually). Now, `--defer-db-indexes` will systematically make sure that indexes are dropped when set. Thus, this flag effectively controls the existence (or more specifically, the absence) of query indexes in the database. Starting kupo without this flag still creates the indexes should they not exist.

#### Removed

N/A

### [2.3.0] - 2023-01-05

#### Added

- Support for [ETag/If-None-Match](https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching#etagif-none-match) standard HTTP caching. `ETag` in kupo matches the most recent block header hash. This allows clients to perform efficient polling and caching on their end. This also comes as an additional protection for rollbacks as one can control the `ETag` between two requests and assess whether a rollback happened.

- [📌 #96](https://github.com/CardanoSolutions/kupo/issues/96) Matches can now be fetched in a paginated fashion, using slot ranges. Ranges can be made on either `created_at` or `spent_at` fields, and are inclusive. Besides, clients have two ways to define ranges:

  - by absolute slot number;
  - by point (slot number + block header hash).

  The latter performs an extra check and will fail should the provided point not exist. This is handy to fetch collections over multiple pages while ensuring that the underlying data doesn't change due to a new fork of the chain being adopted behind the scene. Here are some examples of queries with (valid) ranges:

  ```
  /matches?created_after=1234
  ```

  ```
  /matches?created_after=1234&created_before=5678
  ```

  ```
  /matches?spent&spent_before=1234
  ```

  ```
  /matches?spent&created_after=1234.4675360c80235b60b127222702b6e9b2b5c20dee7115acfc46eb6f3e9fd97ff0&spent_before=5678
  ```

  See:
    - `GET /matches` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getAllMatches)
    - `GET /matches/{pattern}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches)

#### Changed

- [📌 #94](https://github.com/CardanoSolutions/kupo/issues/94) Improved user-experience on start-up when providing invalid or missing working directory. Kupo will now recursively create the working directory if it's missing and otherwise provide a more informative error if it can't (e.g. because the directory already exists and is a file or because of a lack of permissions).

- [📌 #95](https://github.com/CardanoSolutions/kupo/issues/95) The behavior associated with `rollback_to` (when dynamically inserting new patterns) has been slightly altered. Before, it used to fail when providing only a slot number not associated with any on-chain point. Now, Kupo will rollback to the closest ancestor of the given slot number. When there's an existing point, or when a header-hash is provided alonside the slot number; the behavior remains unchanged.

- [📌 #98](https://github.com/CardanoSolutions/kupo/issues/98) Logs are now shown in a human-friendly way when `stdout` is an ANSI-capable terminal. When sent to a file or to a non-terminal, the behaviour is unchanged (i.e. structured JSON).

- Kupo will no longer crash when loosing connection with the underlying cardano-node or Ogmios server. Instead, it'll recover from the issue and try to reconnect automatically.

#### Removed

- The `/patterns` family of endpoints no longer return a `X-Most-Recent-Checkpoint` for it doesn't make much sense for these endpoints. Indeed, they may change regardless of what is processed by the indexer.

### [2.2.1] - 2022-11-26

#### Added

N/A

#### Changed

- [📌 #86](https://github.com/CardanoSolutions/kupo/issues/86) - Fixed start-up configuration check to not fail when providing twice the same pattern on restart.

- Fix Ogmios' parser for non-Babbage eras in the presence of collaterals without collateral return.

- Provide better error message when starting Kupo with a misconfigured network. This is a pretty common case and the message used to be a bit cryptic. Kupo now detects that and inform the end-user properly.

#### Removed

N/A

### [2.2.0] - 2022-11-16

#### Added

- Results now contain a new field `datum_type` only present when `datum_hash` is **not** null. It indicates whether the datum in the output is `inline` or only a `hash` reference.

  See `GET /matches/{pattern}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches)

#### Changed

- Fixed a bug where utxo entries from collateral returns (on phase-2 failed transaction) would be missing from the index. In fact, Kupo does not index outputs from failed transaction, but since the Babbage era, failed transactions may contain an extra field "collateral return" which becomes a legitimate transaction output on-chain. These are now properly indexed as well.

#### Removed

N/A

### [2.1.0] - 2022-10-22

> **Warning**
> **Internal Breaking-Change**
>
> This release internally reworks the `output_reference` column of the `inputs` table.
> There's no direct upgrade from an existing index since this requires additional information
> not present in the database; the only possible migration is therefore to drop the entire index
> and force a resync from scratch.

#### Added

- [📌 #25](https://github.com/CardanoSolutions/kupo/issues/25) - New pattern format to match results by transaction id or by output reference. Also added two new HTTP query parameters for filtering matches: `transaction_id` & `output_index`. See more information in the [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getAllMatches).

  <details><summary>See syntax</summary>

  ```
    ┏━━━━━━━━━━━━━━┓ ╭───╮ ┏━━━━━━━━━━━━━━━━┓
  ╾─┫ OUTPUT_INDEX ┣─┤ @ ├─┫ TRANSACTION_ID ┣─╼
    ┗━━━━━━━━━━━━━━┛ ╰───╯ ┗━━━━━━━━━━━━━━━━┛
  ```
  </details>

  > **Note**
  > Filtering is done by scanning linearly over all results, whereas matching is much faster and leverages the database's internal indexes. One can however combine filters and matches in all kind of ways.


- [📌 #56](https://github.com/CardanoSolutions/kupo/issues/56) - New pattern format to match results by policy id or asset id. Also added two new HTTP query parameters for filtering matches: `policy_id` & `asset_name`. See more information in the [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getAllMatches).

  <details><summary>See syntax</summary>

  ```
    ┏━━━━━━━━━━━┓ ╭───╮ ┏━━━━━━━━━━━━┓
  ╾─┫ POLICY_ID ┣─┤ . ├─┫ ASSET_NAME ┣─╼
    ┗━━━━━━━━━━━┛ ╰───╯ ┗━━━━━━━━━━━━┛
  ```
  </details>

  > **Note**
  > Filtering is done by scanning linearly over all results, whereas matching is much faster and leverages the database's internal indexes. One can however combine filters and matches in all kind of ways.

- [📌 #51](https://github.com/CardanoSolutions/kupo/issues/51) - New endpoint to retrieve transaction metadata by slot number, possibly filtered by transaction id or output reference.

  - `GET /metadata/{slot-no}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMetadataBySlot)

- [📌 #63](https://github.com/CardanoSolutions/kupo/issues/63) - Prometheus-compatible format can now be returned by `GET /health`. This occurs when `Accept: text/plain` is provided as request header. Otherwise, it defaults to `application/json` as before.

- [📌 #65](https://github.com/CardanoSolutions/kupo/issues/65) - Added `transaction_index` to each result, referring to the index in the block of the transaction carrying the matched output.

- [📌 #72](https://github.com/CardanoSolutions/kupo/pulls/72) - Clients can now bulk-add many patterns at once.

  - `PUT /patterns` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/putPatterns)

- New command-line flag `--defer-db-indexes` to be given during the **very first** synchronization to postpone the creation of some non-essential database indexes. Non-essential refers to indexes that are needed to speed up some queries (e.g. querying by payment credentials) but that do not play a role in synchronization. In fact, maintaining such indexes during synchronizations (millions of blocks) adds a lot of unnecessary overhead. Indexes can be created after the facts in a couple of minutes on a standard laptop. This flag therefore comes in handy to speed up an initial synchronization. Once synchronized, the server can be restarted without the flag to automatically create the necessary indexes.

- The server now makes use of an internal resource pool when it comes to database connections. This pool can be configured using a new command-line option `--max-concurrency`. Users running an instance of Kupo on a capable machine (e.g. 32-cores) may want to increase the default of 50.

#### Changed

- [📌 #65](https://github.com/CardanoSolutions/kupo/issues/65) - Results are now ordered by descending slots, transaction index and output index by default. They used to be ordered by descending slot number only; and then arbitrarily ordered from within a same slot. The order is now fully stable.

- [📌 #58](https://github.com/CardanoSolutions/kupo/issues/58) - Improved performances of query by payment credentials from linear times to logarithmic times. Most patterns now benefits from database indexes and are blazing fast.

- Tuned various runtime and internal parameters to optimize query performances. The indexer now makes better use of available machine resources -- in particular CPU.

- Request log messages of the HTTP servers have been split in two; the server is now logging request and responses independently with the time taken to process the response as part of the log message.

- The server now returns an error `503 Service Unavailable` when too many requests are pilling up. Note that the server can handle a relatively heavy load for most patterns but, for large addresses (such as the contract addresses of big marketplaces or DEXes), a single query can take a few seconds. If the server has exhausted all its available resources to serve additional requests, it'll fail gracefully with an error `503` and let the client handle requeuing of the request if necessary.

#### Removed

- `stack` is no longer supported as a development tool / build option.

- A bug in the `PUT /patterns/{pattern-fragment}` endpoint which would cause the server to take a very long time to reply when already synchronized. Adding a pattern is now instantenous when connected through Ogmios and effective as soon as a next block is visible when connected through cardano-node.

### [2.0.0-beta] - 2022-08-07

> **Warning**
> **Breaking-Changes**
>
> This release contains important changes in the database structure and is
> therefore not compatible with previous releases. A full re-synchronization of
> the index will be needed.
>
> Note also that any intermediate work from `master` isn't guaranteed to be
> compatible with `v2.0.0`. Should you have been using intermediate edge versions,
> you will need, in all likelihood, to drop and reconstruct the database as well.

#### Added

- [📌 #28](https://github.com/CardanoSolutions/kupo/issues/28) - Support for synchronization through the Babbage's era, including capturing inline-datums & reference scripts.

- [📌 #17](https://github.com/CardanoSolutions/kupo/issues/20) - New command-line flag: `--prune-utxo`. When set, inputs that are spent on-chain will be removed from the index. Once-synced,
  the index therefore only contain the current ledger UTxO set. When not set, spent inputs are kept in the index but are now marked accordingly to record if and when they've spent.

  HTTP endpoints for `/matches` (& the like) can also now accept either optional query-flag `?spent` or `?unspent` to filter matches depending on whether they've been spent.

  Consequently, there's also a new (possibly `null`) field `spent_at` returned for each match result. When set, it indicates the slot in which the input was found being spent.

  - `GET /matches[?(spent|unspent)]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getAllMatches)
  - `GET /matches/{pattern-fragment}[?(spent|unspent)]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches1Ary)
  - `GET /matches/{pattern-fragment}/{pattern-fragment}[?(spent|unspent)]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches2Ary)

<br/>

- [📌 #21](https://github.com/CardanoSolutions/kupo/issues/21) New HTTP endpoint to retrieve Plutus' datum pre-image from a datum hash digest. Behind the scene, Kupo now track any datum found in transactions' witnesses set or output (inline datums). Note that, datums that aren't associated to any existing pattern matches are eventually garbage-collected.
  - `GET /datums/{datum-hash}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getDatumByHash)

<br/>

- [📌 #21](https://github.com/CardanoSolutions/kupo/issues/28) New HTTP endpoint to retrieve native & Plutus' script pre-image from a script hash digest. Behind the scene, Kupo now track any script found in transactions' witnesses set, auxiliary data and/or outputs (reference scripts).
  - `GET /scripts/{script-hash}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getScriptByHash)

<br/>

- [📌 #40](https://github.com/CardanoSolutions/kupo/issues/40) New HTTP endpoint to retrieve patterns that _includes_ a given pattern. Useful to check if an address is matched by a given configuration.
  - `GET /patterns/{pattern-fragment}[/{pattern-fragment}]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/matchPattern1Ary)

<br/>

- [📌 #40](https://github.com/CardanoSolutions/kupo/issues/21) New optional command-line option `--gc-interval` to tweak the interval between database's garbage collection. Each garbage collection takes a bit of time (few seconds) and pauses the indexer while doing so; A too short interval may have a strong impact on overall syncing time. A too long interval may increase the time needed for collecting garbage. Optimal values depends on your use-case, but the default (10 minutes) is seemingly a sensible default. While syncing from scratch with very permissive patterns, you may want to increase this value (e.g. to 60 minutes) to avoid needlessly pausing the synchronization.

<br/>

- [📌 #24](https://github.com/CardanoSolutions/kupo/issues/24) - New HTTP endpoint to retrieve a point on-chain from a given slot. The endpoint is flexible and allows for retrieving the ancestor
  of a known point very easily. This is handy in combination with other protocols that leverage on-chain points and intersections (like [Ogmios' chain-sync](https://ogmios.dev/mini-protocols/local-chain-sync/)).

  - `GET /checkpoints/{slot-no}[?strict]` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getCheckpointBySlot)

<br/>

- [📌 #35](https://github.com/CardanoSolutions/kupo/issues/35) - New HTTP header `X-Most-Recent-Checkpoint` to every (successful) response. It contains the slot number of the current database most recent checkpoint. This allows client to know which slot a certain query is accurate of.

#### Changed

- [📌 #17](https://github.com/CardanoSolutions/kupo/issues/20) - The `slot_no` and `header_hash` fields are no longer accessible on top-level match result objects. Instead, they're now nested under a `created_at` field, analogous to how `spent_at` has been introduced.

- [📌 #24](https://github.com/CardanoSolutions/kupo/issues/24) - Fixed a bug where listing checkpoints would sometimes return duplicate entries.

- [📌 #39](https://github.com/CardanoSolutions/kupo/issues/39) - Inserting a new pattern (i.e. `PUT /patterns/{pattern-fragment}`) now requires to provide a rollback point, to which the server will rollback and start synchronizing again. The old behavior can be recovered by simply passing the most recent checkpoint as a rollback point. Note that, you may add an already existing pattern if you only need, for some reason, to rollback the indexer to some previous point in time. See the [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/putPattern1Ary) for details.

- [📌 #48](https://github.com/CardanoSolutions/kupo/discussions/48) - (Massively) improved performance of query by stake credential. This used to be in linear time in the size of the UTxO set, and is now performed in logarithmic time, same as query by address. Querying by payment credentials is still performed in linear times though so this is probably something you want to avoid doing on permissive patterns (e.g. `*` or `*/*`).

- API endpoints are no longer versioned (i.e. prefixed with `v1`). However, providing `v1` will still work and route requests all-the-same to ensure backward-compatibility. The rationale being that, since kupo is a local service (and thus, clients decide when they want to upgrade), there's no particular need to version the API in the request path.

- Fixed a bug where the server would systematically reject any request to dynamically remove a pattern (because deemed overlapping with existing patterns).

#### Removed

- N/A

### [1.0.1] - 2022-05-04

#### Added

N/A

#### Changed

- [📌 #17](https://github.com/CardanoSolutions/kupo/discussions/17) - The internal reconnection logic and chain provider error handling has been reworked to be more resilient. In particular, before this patch, Kupo would re-synchronize the index from the provided configuration point in case of a connection lost and recovered with the chain provider. Now, it restarts where it was
before the connection was lost. Also, for Ogmios, few exceptions weren't properly caught and would simply cause the server to crash when loosing connection.

- Fixed a log message informing about ongoing migration, communicating a wrong target version being migrated to (despite doing migration correctly).

#### Removed

- The server will no longer match on failed Alonzo transactions. That is, the index is only constructed out of successful and valid transactions. On Mainnet, this only concerns one transaction, but this could lead to quite surprising behavior should an application be watching the chain via Kupo as an index. Consequently, `1.0.1` includes [an automatic database migration](https://github.com/CardanoSolutions/kupo/blob/3d1ed9be402a7fc2129035d5f98fbb9b9a060c93/db/004.sql) which will prune any concerned output reference from the database for both _mainnet_ and _testnet_.

### [1.0.0] - 2022-03-27

#### Added

- [📌 #1](https://github.com/CardanoSolutions/kupo/issues/1) - New API endpoint to get application's health,
  - `GET v1/health` → [🕮  API Reference](https://cardanosolutions.github.io/kupo/#operation/getHealth)

- [📌 #1](https://github.com/CardanoSolutions/kupo/issues/1) - New command `healthcheck` to perform a health check against a running server. Handy when combined with Docker's HEALTHCHECK feature.
- [📌 #9](https://github.com/CardanoSolutions/kupo/issues/9) - Add `header_hash` to each match, so that data can easily be queried from [Ogmios](https://github.com/CardanoSolutions/ogmios) using the chain-sync protocol (which requires _points_ defined as both slot number and header hashes!).

- [📌 #11](https://github.com/CardanoSolutions/kupo/issues/11) - Support for [Ogmios](https://github.com/CardanoSolutions/ogmios) as an alternative chain-sync provider. This is particularly interesting when used with remote instances. This is activated by providing the relevant command-line options instead of `--node-socket` and `--node-config`:
  - `--ogmios-host`
  - `--ogmios-port`

- [📌 #3](https://github.com/CardanoSolutions/kupo/issues/3) - Support for dynamic management (via HTTP API) of patterns. It is not possible to add, remove and list existing patterns via the HTTP API:
    - `GET v1/patterns` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getPatterns)
    - `DELETE v1/patterns/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/deletePattern1Ary)
    - `DELETE v1/patterns/{pattern-fragment}/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/deletePattern1Ary)
    - `PUT v1/patterns/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/putPattern1Ary)
    - `PUT v1/patterns/{pattern-fragment}/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/putPattern1Ary)

- [📌 #10](https://github.com/CardanoSolutions/kupo/issues/10) - Allow purging old matches (via HTTP API). This is however only allowed if the provided pattern is strictly non overlapping with an existing (active) pattern.
    - `DELETE v1/matches/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/deleteMatch1Ary)
    - `DELETE v1/matches/{pattern-fragment}/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/deleteMatch1Ary)

#### Changed

- [📌 #2](https://github.com/CardanoSolutions/kupo/issues/2) - The command-line is now more idempotent, restarting the server with the same options will no longer fail and simply resume syncing.

- [📌 #13](https://github.com/CardanoSolutions/kupo/issues/13) - Kupo no longer ignores Byron blocks internally and will now also synchronizes data from them.

- [📌 #8](https://github.com/CardanoSolutions/kupo/issues/8) - Protects against restarting with different, non-compatible patterns. In case one restart the server with different patterns that those previously provided, it'll abort and crash. Note however that it's possible to restart the server with an empty patterns set, in which case it uses the previously known ones.

### [1.0.0-beta] - 2022-02-27

#### Added

- Initial (beta) release:
  - Synchronization from chosen point (`--since POINT`)

  - Storage in-memory or on-disk (`--workdir DIR | --in-memory`)

  - Multiple patterns via the command-line (`--match`)

  - Basic API access:
    - `GET v1/matches` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getAllMatches)
    - `GET v1/matches/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches1Ary)
    - `GET v1/matches/{pattern-fragment}/{pattern-fragment}` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getMatches2Ary)
    - `GET v1/checkoints` → [📖 API Reference](https://cardanosolutions.github.io/kupo/#operation/getCheckpoints)

  - Multi-level (basic) structured component logging (`--log-level`, `log-level-{component}`)

- Current limitations:

  - Byron blocks are ignored;
  - The HTTP server does not support `OPTION` and `HEAD` requests;
  - Lack of proper integration testing;

#### Changed

N/A

#### Removed

N/A
