## Overview

<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./schema-dark.png">
    <img alt="Database schema." src="./schema-light.png">
  </picture>
</p>

<details>
  <summary>See complete definition</summary>

```sql
CREATE TABLE `inputs` (
  `output_reference` BLOB PRIMARY KEY NOT NULL,
  `address` TEXT NOT NULL,
  `value` BLOB NOT NULL,
  `datum_hash` BLOB,
  `created_at` INTEGER NOT NULL,
  `spent_at` INTEGER
);
CREATE INDEX `inputsByAddress` ON `inputs` (`address`, `spent_at`);


CREATE TABLE `checkpoints` (
  `header_hash` BLOB NOT NULL,
  `slot_no` INTEGER PRIMARY KEY NOT NULL
);
CREATE INDEX `checkpointsBySlot` ON `checkpoints` (`slot_no`);

CREATE TABLE `binary_data` (
  `binary_data_hash` BLOB PRIMARY KEY NOT NULL,
  `binary_data` BLOB NOT NULL
);
CREATE INDEX `binaryDataByHash` ON `binary_data` (`binary_data_hash`);

CREATE TABLE `patterns` (
  `pattern` TEXT PRIMARY KEY NOT NULL
);
```
</details>

## Changelog

<p align="right"><code>v2.0.0</code></p>
<hr/>

### Migration to `version=7`

- New table `binary_data` indexing binary data by hashes.

### Migration to `version=6`

- Set `synchronous = normal` 

### Migration to `version=5`

- Add columns `spent_at`;
- Rename column `slot_no` into `created_at`;
- Remove column `header_hash` (now obtain via joining on the `checkpoints` table);
- Add compound index (address, spent_at) for `inputs`;
- Add index on `slot_no` for `checkpoints`.

<p align="right"><code>v1.0.1</code></p>
<hr/>

### Migration to `version=4`

- Removes all matches of mainnet & testnet transactions that failed phase-2 validations and consumed collaterals. 

<p align="right"><code>v1.0.0</code></p>
<hr/>

### Migration to `version=3`

- Add a new `patterns` table to support (a) persisting patterns configuration and (b) managing patterns dynamically via HTTP.

### Migration to `version=2`

- Add a new `header_hash` column to the inputs table (to associate matches with fully qualified points made of slot and block header hash). 
