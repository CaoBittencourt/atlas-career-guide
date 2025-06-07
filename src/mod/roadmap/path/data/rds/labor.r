# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  dplyr[...]
)

# endregion
# region: data
# ids
getOption("atlas.oldata") |>
  read.csv() |>
  select(
    id_soc_code,
    occupation
  ) |>
  arrange(
    occupation
  ) |>
  mutate(
    .before = 1,
    id = row_number()
  ) |>
  inner_join(
    # labor statistics
    getOption("atlas.labor") |>
      readRDS()
  ) |>
  select(
    id,
    occupation,
    w = employment_variants,
    wage
  ) ->
df_labor

# endregion
# export
# region: exports
df_labor |> saveRDS(getOption("atlas.mod") |> file.path("roadmap", "path", "data", "rds", "labor.rds"))

# endregion
