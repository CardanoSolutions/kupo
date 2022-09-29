DELETE FROM inputs;
DELETE FROM checkpoints;
DELETE FROM binary_data;
DELETE FROM scripts;

CREATE INDEX IF NOT EXISTS inputsByCreatedAt ON inputs(created_at, substr(output_reference, -2));
