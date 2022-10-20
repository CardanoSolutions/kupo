CREATE TABLE IF NOT EXISTS inputs (
  output_reference BLOB NOT NULL,
  address TEXT NOT NULL,
  value BLOB NOT NULL,
  datum_hash BLOB,
  slot_no INTEGER NOT NULL,
  PRIMARY KEY (output_reference)
);

CREATE TABLE IF NOT EXISTS checkpoints (
  header_hash BLOB NOT NULL,
  slot_no INTEGER NOT NULL,
  PRIMARY KEY (slot_no)
);
