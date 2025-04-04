CREATE TABLE IF NOT EXISTS labor (
    pk_occupation_id INTEGER PRIMARY KEY,
    employment INTEGER CHECK (employment > 0),
    wage REAL CHECK (wage > 0),
    FOREIGN KEY (pk_occupation_id) REFERENCES occupations (pk_occupation_id)
);