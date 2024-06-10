DELETE FROM checkpoints;
DELETE FROM policies;
DELETE FROM inputs;

DROP INDEX IF EXISTS inputsByCreatedAt;
DROP INDEX IF EXISTS inputsBySpentAt;
DROP INDEX IF EXISTS inputsByDatumHash;
DROP INDEX IF EXISTS inputsByAddress;
DROP INDEX IF EXISTS inputsByPaymentCredential;

ALTER TABLE inputs RENAME COLUMN datum_hash TO datum_info;
ALTER TABLE inputs ADD COLUMN datum_hash BYTEA GENERATED ALWAYS AS (substr(datum_info, 2)) STORED;
