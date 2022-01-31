<p align="center">
  <img src=".github/kupo.png" height=120 />
</p>

**Kupo** is a daemon and HTTP server that synchronizes with the Cardano blockchain to build a lookup index for selected entities of the chain; for example, output references associated to addresses. It is a lightweight and portable solution for those who don't want to go down the path of setting up a full-blown database of the entire chain.

# Installation (from source)

```console
$ nix-build -A kupo.components.exes.kupo
```

This produces a statically-linked executable that provides a command-line interface for passing options and commands.

# Getting Started

Currently, Kupo only synchronizes output references from the chain. That is, it creates an index between addresses and outputs references to make it easy to resolve 

```console
$ kupo 
  --node-socket /some/path/node.socket \
  --node-config /some/path/node.config \
  --match "addr_vkh1l66raev2z6rdstm077r7yxh7yyj9wdczsm4l0sh8swalcp350sj^*" \
  --match "addr_vkh1ugfy2ums9pht7lpw0qamlnltg0h93gtgdkp0dlmc0cs67f08ycr^*"
```

This will build an index for all addresses matched by the patterns. Patterns are of the form 

```
{address} | ({hash}|*)[^({hash}|*)] 
```

where `{address}` is an address and `{hash}` is a bech32 (or base16) hash digest of a verification key or script or a wildcard matching everything. Patterns are additive, so specifying multiple ones will increase the number of addresses being matched. Also, they come in two parts, as it is possible to match for either of (or both) the payment part of the delegation part of an address. Incidentally, it is possible to match for all address by using `*` as a pattern. 

For example, here are a few valid patterns:

- `*`
- `*^*`
- `script1l66raev2z6rdstm077r7yxh7yyj9wdczsm4l0sh8swalcaydj5n`
- `script1l66raev2z6rdstm077r7yxh7yyj9wdczsm4l0sh8swalcaydj5n^*`
- `addr_vkh1l66raev2z6rdstm077r7yxh7yyj9wdczsm4l0sh8swalcp350sj-stake_vkh1l66raev2z6rdstm077r7yxh7yyj9wdczsm4l0sh8swalcl9gf43`
- `*-stake_vkh1l66raev2z6rdstm077r7yxh7yyj9wdczsm4l0sh8swalcl9gf43`

Once the daemon is started, it'll synchronize with the chain to build an index for matching entities. One can then query entities via HTTP GET requests, using patterns 

```console
$ curl http://localhost:1442/addr1qq7390qwpw8vjc4q5yeus6trhr4r6kmet0x3fmm9key6y9xda2j73jmxarz2kv60uqunlgh2fgg3yjkvxspnjkdx2j0sc7e9xp | jq
[ 
  { 
    "address": "addr1qq7390qwpw8vjc4q5yeus6trhr4r6kmet0x3fmm9key6y9xda2j73jmxarz2kv60uqunlgh2fgg3yjkvxspnjkdx2j0sc7e9xp"
    "txId": "a78f366f272f20101f116f932a58d5157dc41696e618b6bfcf011909326a384f",
    "outputIndex": 1
    "value": {
      "coins": "...",
      "assets": {}
    },
    "datum": "..."
  }
]
```

---

<p align="center">
  <a href="CONTRIBUTING.md">:triangular_ruler: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>

<p align="center"><a href="https://github.com/cardanosolutions/kupo/blob/master/LICENSE"><img src=".github/license.svg" alt="license=MPL-2.0" /></a></p>
