PRAGMA user_version = 1;
PRAGMA journal_mode = WAL;

CREATE TABLE IF NOT EXISTS inputs (
  output_reference BLOB NOT NULL,
  address BLOB NOT NULL,
  value BLOB NOT NULL,
  datum_hash BLOB,
  slot_no INTEGER NOT NULL,
  PRIMARY KEY (output_reference)
);

CREATE INDEX IF NOT EXISTS inputsByAddress ON inputs(address);

CREATE TABLE IF NOT EXISTS addresses (
  payment BLOB NOT NULL,
  delegation BLOB
);

CREATE INDEX IF NOT EXISTS addressByPayment ON addresses(payment);
CREATE INDEX IF NOT EXISTS addressByDelegation ON addresses(delegation);
CREATE UNIQUE INDEX IF NOT EXISTS addressByPaymentAndDelegation ON addresses(payment, delegation);

CREATE TABLE IF NOT EXISTS checkpoints (
  header_hash BLOB NOT NULL,
  slot_no INTEGER NOT NULL,
  PRIMARY KEY (slot_no)
);
