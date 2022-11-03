DELETE FROM checkpoints WHERE slot_no >= (SELECT MIN(created_at) FROM inputs WHERE datum_hash IS NOT NULL);
DELETE FROM inputs      WHERE created_at > (SELECT MIN(created_at) FROM inputs WHERE datum_hash IS NOT NULL);
CREATE INDEX IF NOT EXISTS inputsBySpentAt ON inputs(spent_at);
UPDATE inputs SET spent_at = NULL WHERE spent_at >= (SELECT MIN(created_at) FROM inputs WHERE datum_hash IS NOT NULL);
DROP INDEX inputsBySpentAt;
DELETE FROM inputs WHERE datum_hash IS NOT NULL;

ALTER TABLE inputs RENAME COLUMN datum_hash TO datum_info;
ALTER TABLE inputs ADD COLUMN datum_hash BLOB GENERATED ALWAYS AS (substr(datum_info, 2)) VIRTUAL;
