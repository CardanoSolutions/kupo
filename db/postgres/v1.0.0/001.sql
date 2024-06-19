SET client_min_messages TO WARNING;

DROP TABLE inputs;

CREATE TABLE IF NOT EXISTS inputs (
  output_reference BYTEA NOT NULL,
  address TEXT NOT NULL,
  value BYTEA NOT NULL,
  datum_hash BYTEA,
  header_hash BYTEA NOT NULL,
  slot_no INTEGER NOT NULL,
  PRIMARY KEY (output_reference)
);

DELETE FROM checkpoints;
