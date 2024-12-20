CREATE VIEW IF NOT EXISTS soc_variants AS
SELECT
    pk_occupation_id,
    COUNT(*) OVER (
        PARTITION BY
            soc_code
    ) AS soc_variants
FROM
    occupations
ORDER BY
    occupation;