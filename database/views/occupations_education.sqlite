CREATE VIEW IF NOT EXISTS occupations_education AS
SELECT
    occupation,
    education,
    education_years
FROM
    occupations
    INNER JOIN required_education ON occupations.pk_occupation_id = required_education.pk_occupation_id
    INNER JOIN education ON required_education.fk_education_id = education.pk_education_id
ORDER BY
    occupation;