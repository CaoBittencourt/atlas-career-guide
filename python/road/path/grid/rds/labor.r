# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  dplyr[...],
  roadmap / path / data / req[df_ids]
)

# endregion
# region: data
# ids
df_ids |>
  left_join(
    # labor statistics
    Sys.getenv("ATLAS_LABOR") |>
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
df_labor |> saveRDS(Sys.getenv("ATLAS_MOD") |> file.path("roadmap", "path", "data", "rds", "labor.rds"))

# endregion
