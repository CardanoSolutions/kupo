### [1.0.0-beta] - 2022-02-27

#### Added

- Initial (beta) release:
  - Synchronization from chosen point (`--since POINT`)

  - Storage in-memory or on-disk (`--workdir DIR | --in-memory`)

  - Multiple patterns via the command-line (`--match`)

  - Basic API access:
    - `GET v1/matches`
    - `GET v1/matches/{pattern-fragment}`
    - `GET v1/matches/{pattern-fragment}/{pattern-fragment}`
    - `GET v1/checkoints`

  - Multi-level (basic) structured component logging (`--log-level`, `log-level-{component}`)

- Current limitations:
  
  - Byron blocks are ignored;
  - The HTTP server does not support `OPTION` and `HEAD` requests;
  - Lack of proper integration testing;

#### Changed

N/A

#### Removed

N/A
