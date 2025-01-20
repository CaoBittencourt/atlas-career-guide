CREATE TABLE IF NOT EXISTS required_education (
    pk_occupation_id INTEGER PRIMARY KEY,
    fk_education_id INTEGER,
    FOREIGN KEY (pk_occupation_id) REFERENCES occupations (pk_occupation_id),
    FOREIGN KEY (fk_education_id) REFERENCES education (pk_education_id)
);