CREATE TABLE IF NOT EXISTS binary_data (
  binary_data_hash BLOB NOT NULL,
  binary_data BLOB NOT NULL,
  PRIMARY KEY (binary_data_hash)
);

CREATE INDEX IF NOT EXISTS binaryDataByHash ON binary_data(binary_data_hash);
