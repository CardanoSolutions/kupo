ALTER TABLE inputs ADD COLUMN sub_transaction_index INTEGER;
ALTER TABLE inputs ADD COLUMN top_level_transaction_id BLOB;
ALTER TABLE inputs ADD COLUMN spent_by_top_level_transaction_id BLOB;
