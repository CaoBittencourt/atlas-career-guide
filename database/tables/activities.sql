CREATE TABLE IF NOT EXISTS activities (
    pk_occupation_id INTEGER,
    pk_item_id INTEGER,
    item_score REAL CHECK (item_score BETWEEN 0 AND 1),
    PRIMARY KEY (pk_occupation_id, pk_item_id),
    FOREIGN KEY (pk_occupation_id) REFERENCES occupations (pk_occupation_id),
    FOREIGN KEY (pk_item_id) REFERENCES attributes (pk_item_id)
);