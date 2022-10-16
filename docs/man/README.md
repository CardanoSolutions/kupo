% KUPO
% Matthias Benkort <matthias.benkort+kupo@cardanofoundation.org>
% September 2022

# NAME
kupo - Fast, lightweight & configurable chain-index for Cardano.

# SYNOPSIS
**kupo**\
    (--node-socket *FILEPATH* --node-config *FILEPATH* | --ogmios-host *IPv4* --ogmios-port *TCP/PORT*)\
    (--workdir *DIRECTORY* | --in-memory)\
    [--host *IPv4*] [--port *TCP/PORT*]\
    [--since *POINT*]\
    [--match *PATTERN*]\
    [--prune-utxo]\
    [--gc-interval *SECONDS*]\
    [<u>log-level</u>]

**kupo** ((-v|--version) | **version**)

**kupo** **health-check**

# DESCRIPTION
**Kupo** is fast, lightweight and configurable chain-index for the **Cardano** blockchain. **Kupo** synchronizes data from the blockchain according to <u>patterns</u> matching *addresses* present in transaction outputs and builds a lookup table from matches to their associated *output references*, *values*, *datums* and *scripts*. All indexed data is made available via a local HTTP web-server.

# OPTIONS
**--node-socket**
:   Path to the Cardano node domain socket file.

    (**NOTE**: Unused when connecting to Ogmios)

**--node-config**
:   Path to the Cardano node configuration file.

    (**NOTE**: Unused when connecting to Ogmios)

**--ogmios-host**
:   Ogmios' host address.

    (**NOTE**: Unused when connecting to a Cardano node)

**--ogmios-port**
:   Ogmios' port.

    (**NOTE**: Unused when connecting to a Cardano node)

**--workdir**
:   Path to a working directory, where the SQLite database files are stored. By convention, the database is called <u>kupo.sqlite</u>.

    (**NOTE**: Unused when running **in-memory**)

**--in-memory**
:   Run fully in-memory, data is short-lived and lost when the process exits. This mode is not suitable for long-lived database using a permissive index pattern (e.g. `*`).\

    (**NOTE** Unused when a **workdir** is specified.)

**--host**
:   Address to bind the HTTP web-server to.

    (default: *"127.0.0.1"*)

**--port**
:   Port the HTTP web-server listen on.

    (default: *1442*)

**--since**
:   A point on chain from where to start syncing.

    Expects either:

    - *origin*
    - A dot-separated integer (absolute slot number) and base16-encoded hash digest (block header hash).

    This option is *mandatory* when starting the application for a *first-time*. Afterwards, it becomes optional. If specified again however, it *must* not be conflicting with the original value.

**--match**
:   A pattern to match on. Can be provided multiple times (as a logical disjunction, i.e. 'or').\

    See [**Patterns**](#Patterns) below.

**--prune-utxo**
:   Remove inputs from the index when spent, instead of marking them as 'spent'. Note that most recent inputs
    are not removed immediately, but rather after a certain delay -- when it becomes actually safe to remove
    them. This delay depends on the consensus algorithm and protocol parameters. Currently, it is exactly
    129600 slots (36 hours).

    (default: *disabled*).

**--gc-interval**
:   Number of seconds between background database garbage collections pruning obsolete or unnecessary data. Garbage collections are typically short (few seconds or less) and short remain unnoticed. This parameter exists however to give some level of control if necessary.

    (default: *600s*).

**--max-concurrency**
:   Maximum number of concurrent connections to the database. This also seemingly refer to the maximum number of client requests that the server can handle in parallel, beyond which the server will return *503 Service Unavailable* errors as response to new requests.

    The more cores are available on the host machine, the higher can this number be in theory. The default is sensible for a medium hardware.

    (default: *50*).

**--defer-db-indexes**
:   When enabled, defer the creation of database indexes to the next start. This is useful to make the first-ever synchronization faster but will make certain queries considerably slower.

    Indeed, Kupo makes a heavy use of database indexes on columns and virtual columns which can increase the overall synchronization time by a factor 2 or 3. Maintaining these indexes during synchronization has little benefits and can therefore be postponed to only after the database has been fully synchronized. Consider restarting the application without that flag once synchronized.

    This flag has no effect if a database with indexes already exists. So make sure to turn it on for the very first run.

    (default: *disabled*).

**--log-level**
:   Minimal severity of all log messages.

    - *Debug*
    - *Info*
    - *Notice*
    - *Warning*
    - *Error*

    Or alternatively, to turn a logger off:

    - *Off*

    It is also possible to tweak the log-level on a per-component basis using **--log-level-{component}**.

    The following components are supported:

    - **database**: internal logs about specific database operations.
    - **http-server**: inbound requests made to the http server.
    - **consumer**: report about the internal syncing progress.
    - **garbage-collector**: periodically run to remove now-obsolete data from the database.
    - **configuration**: inform about the applications settings and state on start-up.

# PATTERNS
**Kupo** utilizes patterns to filter entities in blocks fetched from the chain. Patterns apply to certain elements of transactions, such as addresses of their outputs. Some patterns are comprised of several parts with a specific separator. Most parts also accept a wildcard symbol (`*`). One can specify multiple (possibly overlapping) patterns to match many different entities.

**PATTERN**

```
   ┏━━━━━━━━━┓
╾┬─┫ ADDRESS ┣───────────────────────────────┬╼
 │ ┗━━━━━━━━━┛                               │
 │ ┏━━━━━━━━━━━━━━━┓                         │
 ├─┫ STAKE_ADDRESS ┣─────────────────────────┤
 │ ┗━━━━━━━━━━━━━━━┛                         │
 │ ┏━━━━━━━━━━━━┓                            │
 ├─┫ CREDENTIAL ┣────────────────────────────┤
 │ ┗━━━━━━━━━━━━┛                            │
 │ ┏━━━━━━━━━━━━┓   ┏━━━┓ ┏━━━━━━━━━━━━┓     │
 ├─┫ CREDENTIAL ┣───┫ / ┣─┫ CREDENTIAL ┣─────┤
 │ ┗━━━━━━━━━━━━┛   ┗━━━┛ ┗━━━━━━━━━━━━┛     │
 │ ┏━━━━━━━━━━━┓    ┏━━━┓ ┏━━━━━━━━━━━━┓     │
 ├─┫ POLICY_ID ┣────┫ . ┣─┫ ASSET_NAME ┣─────┤
 │ ┗━━━━━━━━━━━┛    ┗━━━┛ ┗━━━━━━━━━━━━┛     │
 │ ┏━━━━━━━━━━━━━━┓ ┏━━━┓ ┏━━━━━━━━━━━━━━━━┓ │
 ├─┫ OUTPUT_INDEX ┣─┫ @ ┣─┫ TRANSACTION_ID ┣─┤
 │ ┗━━━━━━━━━━━━━━┛ ┗━━━┛ ┗━━━━━━━━━━━━━━━━┛ │
 │ ┏━━━┓                                     │
 └─┫ * ┣─────────────────────────────────────┘
   ┗━━━┛
```

**CREDENTIAL**

```
   ┏━━━━━━━━━━━┓
╾┬─┫ 64 HEXDIG ┣───────────────────────────────────────────┬╼
 │ ┗━━━━━━━━━━━┛                                           │
 │ ┏━━━━━━━━━━━━━━━━━━━━━━━━━┓              ┏━━━━━━━━━━━━┓ │
 ├─┫ vk / addr_vk / stake_vk ┣──────────────┫ *BECH32DIG ┣─┤
 │ ┗━━━━━━━━━━━━━━━━━━━━━━━━━┛              ┗━━━━━━━━━━━━┛ │
 │ ┏━━━━━━━━━━━┓                                           │
 ├─┫ 56 HEXDIG ┣───────────────────────────────────────────┤
 │ ┗━━━━━━━━━━━┛                                           │
 │ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓  ┏━━━━━━━━━━━━┓ │
 ├─┫ vkh / addr_vkh / stake_vkh / script ┣──┫ *BECH32DIG ┣─┤
 │ ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛  ┗━━━━━━━━━━━━┛ │
 │ ┏━━━┓                                                   │
 └─┫ * ┣───────────────────────────────────────────────────┘
   ┗━━━┛
```

**POLICY_ID** / **ASSET_NAME**

```
   ┏━━━━━━━━━━━┓
╾┬─┫ 56 HEXDIG ┣─┬╼
 │ ┗━━━━━━━━━━━┛ │
 │ ┏━━━┓         │
 └─┫ * ┣─────────┘
   ┗━━━┛
```

**ASSET_NAME**

```
   ┏━━━━━━━━━━━━━┓
╾┬─┫ 0*64 HEXDIG ┣─┬╼
 │ ┗━━━━━━━━━━━━━┛ │
 │ ┏━━━┓           │
 └─┫ * ┣───────────┘
   ┗━━━┛
```

**OUTPUT_INDEX**

```
   ┏━━━━━━━━━━━┓
╾┬─┫ 1*3 DIGIT ┣─┬╼
 │ ┗━━━━━━━━━━━┛ │
 │ ┏━━━┓         │
 └─┫ * ┣─────────┘
   ┗━━━┛
```

**TRANSACTION_ID**

```
   ┏━━━━━━━━━━━┓
 ╾─┫ 64 HEXDIG ┣─╼
   ┗━━━━━━━━━━━┛
```

**BECH32DIG** = %x61 / %x63-68 / %x6A-6E / %x70-7A / 0-9

**HEXDIG** = %x30–39 / %x41-%46

# EXAMPLES
**kupo**\
  --node-socket /tmp/node.socket \
  --node-config /usr/share/cardano/network/mainnet/cardano-node/config.json \
  --workdir $HOME/.cache/kupo \
  --match * \
  --defer-db-indexes \
  --since origin \
  --prune-utxo

**kupo**\
  --ogmios-host ogmios-api.mainnet.dandelion.link\
  --ogmios-port 443\
  --in-memory\
  --since 16588737.4e9bbbb67e3ae262133d94c3da5bffce7b1127fc436e7433b87668dba34c354a\
  --match addr1vyc29pvl2uyzqt8nwxrcxnf558ffm27u3d9calxn8tdudjgz4xq9p\

# SEE ALSO
Online documentation and API reference: <https://cardanosolutions.github.io/kupo/>

# EXIT VALUES
**0**
: Interrupted by user.

**1**
: Invalid configuration or options.

# BUGS
See and report issues on Github: <https://github.com/CardanoSolutions/kupo/issues>

# COPYRIGHT
Licensed under MPL-2.0.

This program is subject to the terms of the Mozilla Public License, v2.0. If a copy of the MPL was not distributed with this file, You can obtain one at <http://mozilla.org/MPL/2.0/>.
