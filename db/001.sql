/* Used for tracking migrations */
PRAGMA user_version = 1;

CREATE TABLE IF NOT EXISTS addresses (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  network INTEGER NOT NULL, /* 0: testnet, 1: mainnet */
  payment BLOB,
  delegation BLOB,
  bootstrap BLOB
);

CREATE UNIQUE INDEX IF NOT EXISTS addressById ON addresses(id);
CREATE UNIQUE INDEX IF NOT EXISTS addressByPaymentAndDelegation ON addresses(payment, delegation);

CREATE INDEX IF NOT EXISTS addressByPayment ON addresses(payment);
CREATE INDEX IF NOT EXISTS addressByDelegation ON addresses(delegation);
CREATE INDEX IF NOT EXISTS addressByBootstrap ON addresses(bootstrap);

CREATE TABLE IF NOT EXISTS inputs (
  output_reference BLOB PRIMARY KEY NOT NULL,
  address INTEGER NOT NULL,
  value BLOB NOT NULL,
  datum_hash BLOB,
  slot_no INTEGER NOT NULL,
  FOREIGN KEY (address) REFERENCES addresses(id)
);

CREATE UNIQUE INDEX IF NOT EXISTS inputByAddress ON inputs(address);
