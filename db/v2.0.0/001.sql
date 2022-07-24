DROP TABLE inputs;

CREATE TABLE IF NOT EXISTS inputs (
  output_reference BLOB NOT NULL,
  address TEXT NOT NULL,
  value BLOB NOT NULL,
  datum_hash BLOB,
  script_hash BLOB,
  created_at INTEGER NOT NULL,
  spent_at INTEGER,
  PRIMARY KEY (output_reference)
);

CREATE INDEX IF NOT EXISTS inputsByAddress ON inputs(address, spent_at);

CREATE INDEX IF NOT EXISTS inputsByDatumHash ON inputs(datum_hash);

DELETE FROM checkpoints;

CREATE INDEX IF NOT EXISTS checkpointsBySlotNo ON checkpoints(slot_no);

CREATE TABLE IF NOT EXISTS binary_data (
  binary_data_hash BLOB NOT NULL,
  binary_data BLOB NOT NULL,
  PRIMARY KEY (binary_data_hash)
);

CREATE INDEX IF NOT EXISTS binaryDataByHash ON binary_data(binary_data_hash);

CREATE TABLE IF NOT EXISTS scripts (
  script_hash BLOB NOT NULL,
  script BLOB NOT NULL,
  PRIMARY KEY (script_hash)
);

CREATE INDEX IF NOT EXISTS scriptByHash ON scripts(script_hash);
