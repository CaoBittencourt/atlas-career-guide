# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  s = compare / similarity,
  readr[...],
  arrow[write_parquet],
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
Sys.getenv("ATLAS_SKILLS_MTX") |> readRDS() -> df_occupations
Sys.getenv("ATLAS_CAO") |> readRDS() -> df_cao

df_occupations |>
  inner_join(df_cao) |>
  select(-cao) -> df_occupations_cao

# endregion
# model
# region: new dispatch (my matches)
# note: is from/to inverted?
df_cao |>
  s$similarity(
    df_occupations_cao,
    s$similarity.methods$euclidean,
    bind = T
  ) -> similarity_cao

similarity_cao |>
  arrange(desc(cao)) |>
  head(10)

similarity_cao |>
  arrange(desc(cao)) |>
  tail(10)

# endregion
# region: new dispatch (occupations)
# note: is from/to inverted?
df_occupations |>
  s$similarity(
    df_occupations,
    s$similarity.methods$euclidean,
    bind = T
  ) -> similarity

# endregion
# region: parquet
similarity[-1] |>
  write_parquet(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path("parquet", "similarity.parquet")
  )

# endregion
# region: csv
similarity[-1] |>
  write_csv(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path("csv", "similarity.csv")
  )

# endregion
# region: rds
similarity[-1] |>
  saveRDS(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path("rds", "similarity.rds")
  )

# endregion
