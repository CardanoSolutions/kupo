ALTER TABLE inputs ADD COLUMN payment_credential TEXT GENERATED ALWAYS AS (substr(address, -56)) STORED;
