DELETE FROM checkpoints WHERE slot_no >= (SELECT MIN(created_at) FROM inputs WHERE datum_hash IS NOT NULL);
DROP INDEX IF EXISTS inputsByDatumHash;
ALTER TABLE inputs RENAME COLUMN datum_hash TO datum_info;
ALTER TABLE inputs ADD COLUMN datum_hash BLOB GENERATED ALWAYS AS (substr(datum_info, 2)) VIRTUAL;
