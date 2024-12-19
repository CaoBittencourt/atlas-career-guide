CREATE TABLE IF NOT EXISTS occupations (
    pk_occupation_id INTEGER PRIMARY KEY,
    occupation TEXT,
    soc_2int INTEGER NOT NULL CHECK (soc_2int BETWEEN 0 AND 99),
    soc_4int INTEGER NOT NULL CHECK (soc_2int BETWEEN 0 AND 9999),
    soc_code TEXT AS (soc_2int || '-' || soc_4int)
);