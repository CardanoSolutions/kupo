/* Used for tracking migrations */
PRAGMA user_version = 1;

CREATE TABLE IF NOT EXISTS addresses (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  address BLOB
);

CREATE UNIQUE INDEX IF NOT EXISTS addressById ON addresses(id);
CREATE UNIQUE INDEX IF NOT EXISTS addressByAddress ON addresses(address);

CREATE TABLE IF NOT EXISTS inputs (
  output_reference BLOB PRIMARY KEY NOT NULL,
  address INTEGER NOT NULL,
  value BLOB NOT NULL,
  datum_hash BLOB,
  slot_no INTEGER NOT NULL,
  FOREIGN KEY (address) REFERENCES addresses(id)
);

CREATE UNIQUE INDEX IF NOT EXISTS inputByAddress ON inputs(address);
