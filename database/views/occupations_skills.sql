CREATE VIEW IF NOT EXISTS occupations_skills AS
SELECT
    occupation,
    item,
    item_score
FROM
    attributes
    INNER JOIN competencies ON attributes.pk_item_id = competencies.pk_item_id
    INNER JOIN occupations ON competencies.pk_occupation_id = occupations.pk_occupation_id
ORDER BY
    occupation;