CREATE TABLE IF NOT EXISTS migrations (
  id TEXT PRIMARY KEY NOT NULL,
  state TEXT NOT NULL,
  cutoff_slot INTEGER,
  cursor BLOB,
  kupo_version TEXT,
  registered_at INTEGER NOT NULL,
  applied_at INTEGER
);
