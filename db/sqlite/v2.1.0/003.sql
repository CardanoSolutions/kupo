ALTER TABLE inputs RENAME COLUMN output_reference TO ext_output_reference;

ALTER TABLE inputs ADD COLUMN output_reference  BLOB NOT NULL GENERATED ALWAYS AS (substr(ext_output_reference, 1, 34)) VIRTUAL;
ALTER TABLE inputs ADD COLUMN output_index      BLOB NOT NULL GENERATED ALWAYS AS (substr(ext_output_reference, -4, 2)) VIRTUAL;
ALTER TABLE inputs ADD COLUMN transaction_index BLOB NOT NULL GENERATED ALWAYS AS (substr(ext_output_reference, -2))    VIRTUAL;

CREATE UNIQUE INDEX IF NOT EXISTS inputsByOutputReference ON inputs(output_reference);

CREATE TABLE IF NOT EXISTS policies (
  output_reference BLOB NOT NULL,
  policy_id BLOB NOT NULL,
  PRIMARY KEY (output_reference, policy_id),
  CONSTRAINT fk_policies_inputs
    FOREIGN KEY (output_reference)
    REFERENCES inputs(output_reference)
    ON DELETE CASCADE
);
