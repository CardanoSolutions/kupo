DROP TABLE inputs;

CREATE TABLE IF NOT EXISTS inputs (
  output_reference BLOB NOT NULL,
  address TEXT NOT NULL,
  value BLOB NOT NULL,
  datum_hash BLOB,
  created_at INTEGER NOT NULL,
  spent_at INTEGER,
  PRIMARY KEY (output_reference)
);

CREATE INDEX IF NOT EXISTS inputsByAddress ON inputs(address, spent_at);

DELETE FROM checkpoints;

CREATE INDEX IF NOT EXISTS checkpointsBySlotNo ON checkpoints(slot_no);
