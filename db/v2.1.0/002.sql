ALTER TABLE inputs ADD COLUMN payment_credential TEXT COLLATE NOCASE GENERATED ALWAYS AS (substr(address, -56)) VIRTUAL;

CREATE INDEX IF NOT EXISTS inputsByPaymentCredential ON inputs(payment_credential COLLATE NOCASE);
