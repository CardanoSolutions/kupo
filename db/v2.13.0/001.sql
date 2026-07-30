DELETE FROM inputs;

DROP TABLE checkpoints;

CREATE TABLE checkpoints (
  header_hash BLOB NOT NULL,
  slot_no INTEGER PRIMARY KEY NOT NULL,
  block_no INTEGER NOT NULL
);
