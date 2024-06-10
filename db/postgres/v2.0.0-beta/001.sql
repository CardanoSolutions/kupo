DROP TABLE inputs;

CREATE TABLE IF NOT EXISTS inputs (
  output_reference BYTEA NOT NULL,
  address TEXT NOT NULL,
  value BYTEA NOT NULL,
  datum_hash BYTEA,
  script_hash BYTEA,
  created_at INTEGER NOT NULL,
  spent_at INTEGER,
  PRIMARY KEY (output_reference)
);

DELETE FROM checkpoints;

CREATE TABLE IF NOT EXISTS binary_data (
  binary_data_hash BYTEA NOT NULL,
  binary_data BYTEA NOT NULL,
  PRIMARY KEY (binary_data_hash)
);

CREATE TABLE IF NOT EXISTS scripts (
  script_hash BYTEA NOT NULL,
  script BYTEA NOT NULL,
  PRIMARY KEY (script_hash)
);
