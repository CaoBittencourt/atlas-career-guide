modular::project.options("atlas")
getOption("atlas.db") |> setwd()
library(dplyr)
library(tidyr)
read.csv("output/skill_sets.csv") -> skill_set_mtx
read.csv("output/education.csv") -> education
read.csv("output/labor.csv") -> labor

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

read.csv("data/old/occupations/df_occupations_2022.csv") |> as_tibble() -> df_occupations

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
    all(df.wage == wage),
    all(df.employment_variants == employment_variants),
    all(df.education == education),
    all(df.education_years == education_years)
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
