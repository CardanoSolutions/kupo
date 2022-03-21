# Testing Strategy

## End-to-End

Most of the application logic is tested end-to-end in [KupoSpec](./Test/KupoSpec.hs). This set of tests is only executed if the environment variables `CARDANO_NODE_SOCKET` and `CARDANO_NODE_CONFIG` are set and rightfully point to a Cardano testnet node. The node is assumed to by somewhat already synced (at least, up to epoch 192). 

Various scenarios are tested, including:

- failure recovery when the node isn't up-and-running;
- restarting the application with various combination of options;
- in-memory and on-disk indexing...

## Mailbox

The mailbox component (producer/consumer interface) is written using [`io-classes`](https://github.com/input-output-hk/ouroboros-network/tree/master/io-classes) which thereby allows for testing it in simulated IO to look for deadlocks or even, analyze the performance of the pattern: kupo _assumes_ a producer which is much faster than the consumer and [MailboxSpec](./Test/Kupo/App/MailboxSpec.hs) controls that the mailbox patterns performs well with these hypothesis. 

## Http Server

Kupo uses the very idiomatic (in Haskell) [warp](https://hackage.haskell.org/package/warp) and [wai](https://hackage.haskell.org/package/wai) stack for writing the HTTP server. In combination, they allow to decouple the web application logic as pure wai expression and thus, to test it using only pure code. This happens in [HttpSpec](./Test/Kupo/App/HttpSpec.hs). In these tests, the database component is stubbed to produce arbitrary data for every request. More, the test(s) do not only runs requests against a pure Wai application, but they also control that the resulting JSON output abides by Kupo's [OpenAPI specification](../docs/openapi.yaml). Since server outputs are randomly generated, and each test scenario executed hundred of times, this makes for a good coverage of the JSON specification against outputs produced by the server.

## Patterns

Patterns parsing from text and matching are tested through [PatternSpec](./Test/Kupo/PatternSpec.hs) which makes use of pre-defined cases as [Pattern.Fixture](./Test/Kupo/Data/Pattern/Fixture.hs). In the fixtures are define addresses made out of various credentials (scripts, verification key hashes). Then, the test defines a matrix of text patterns  with the addresses they're expected to match. The addresses, credentials and patterns have been chosen to offer a good coverage of the pattern parsing and matching. 

## Database

Types are marshalled to and from the database using specific functions. The [DatabaseSpec](./Test/Kupo/Data/DatabaseSpec.hs) defines roundtrip properties checking that arbitrarily generated data can be marshalled to their database representations, and unmarshalled back to their high-level application form. The module also checks that addresses stored in the database can be fetched correctly via their expected text patterns, translated as SQL(ite) queries. 

> TODO: 
> 
> 1. It would be nice to define a "simple" state-machine test for the database, modelling it using an MVar and throwing random commands at it using its interface to look for subtle bugs which may happen from unforeseen interleaving of commands.
>
> 2. The `Kupo.Control.MonadDatabase` modules holds quite a fair amount of complexity. In particular, it defines a simple approach for handling database migrations. This could be tested in isolation. 

## Command-line & configuration

Command-line options are fully tested in [OptionsSpec](./Test/Kupo/OptionsSpec.hs). The test matrix cover various combination of options, testing out few malformed cases as well. [ConfigurationSpec](./Test/Kupo/ConfigurationSpec.hs) controls that well-formed node configurations and genesis files can be parsed, using those served from [cardano-configurations](https://github.com/input-output-hk/cardano-configurations). In addition, [CardanoSpec](./Test/Kupo/Data/CardanoSpec.hs) also checks using basic properties the parsing from text of a few data-types used in the configuration definition (points, slot, header hash...). 
