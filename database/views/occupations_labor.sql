CREATE VIEW IF NOT EXISTS occupations_labor AS
SELECT
    occupation,
    employment_variants,
    1.0 * employment_variants / (MIN(employment_variants) OVER ()) AS employment_norm,
    wage
FROM
    (
        SELECT
            occupation,
            (employment / soc_variants) AS employment_variants,
            wage
        FROM
            labor
            INNER JOIN soc_variants ON labor.pk_occupation_id = soc_variants.pk_occupation_id
            INNER JOIN occupations ON soc_variants.pk_occupation_id = occupations.pk_occupation_id
    )
ORDER BY
    occupation;