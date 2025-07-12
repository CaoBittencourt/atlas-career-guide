# modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# imports
library(dplyr)
library(tidyr)

# working directory
getOption("atlas.db") |> setwd()

# data
readRDS("output/rds/skill_set_mtx.rds") -> skill_set_mtx
readRDS("output/rds/education.rds") -> education
readRDS("output/rds/labor.rds") -> labor

# check if data matches
skill_set_mtx |>
  inner_join(education) |>
  inner_join(labor) |>
  relocate(
    occupation,
    education,
    education_years,
    starts_with("employment"),
    wage
  ) |>
  as_tibble() ->
occupations

Sys.getenv("ATLAS_OLD_DATA") |>
  read.csv() |>
  as_tibble() ->
df_occupations

occupations |>
  select(
    occupation,
    wage,
    employment_variants,
    education,
    education_years
  ) |>
  inner_join(
    df_occupations |>
      select(
        occupation,
        df.wage = wage,
        df.employment_variants = employment_variants,
        df.education = education,
        df.education_years = education_years
      )
  ) -> dsds

dsds |>
  reframe(
    wage = all(df.wage == wage),
    employment_variants = all(df.employment_variants == employment_variants),
    education = all(df.education == education),
    education_years = all(df.education_years == education_years)
  )

dsds |>
  filter(
    df.employment_variants != employment_variants,
  ) |>
  mutate(
    diff = df.employment_variants - employment_variants
  ) |>
  pull(diff) |>
  summary()

dsds |>
  mutate(
    diff = df.employment_variants - employment_variants
  ) |>
  pull(diff) |>
  summary()
