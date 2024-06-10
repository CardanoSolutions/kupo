CREATE TABLE IF NOT EXISTS inputs (
  output_reference BYTEA NOT NULL,
  address TEXT NOT NULL,
  value BYTEA NOT NULL,
  datum_hash BYTEA,
  slot_no INTEGER NOT NULL,
  PRIMARY KEY (output_reference)
);

CREATE TABLE IF NOT EXISTS checkpoints (
  header_hash BYTEA NOT NULL,
  slot_no INTEGER NOT NULL,
  PRIMARY KEY (slot_no)
);
