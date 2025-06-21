# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  dplyr[...],
  mod / roadmap / path / data / req[df_ids]
)

# endregion
# region: data
# ids
df_ids |>
  left_join(
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
