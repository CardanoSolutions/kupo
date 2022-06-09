DROP TABLE inputs;

CREATE TABLE IF NOT EXISTS inputs (
  output_reference BLOB NOT NULL,
  address TEXT NOT NULL,
  value BLOB NOT NULL,
  datum_hash BLOB,
  header_hash BLOB NOT NULL,
  slot_no INTEGER NOT NULL,
  PRIMARY KEY (output_reference)
);

CREATE INDEX IF NOT EXISTS inputsByAddress ON inputs(address);

DELETE FROM checkpoints;
