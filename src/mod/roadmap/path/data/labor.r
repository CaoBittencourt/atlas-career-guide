# setup
# region: data
readRDS(
  file.path(
    Sys.getenv("ATLAS_MOD"),
    "roadmap",
    "path",
    "data",
    "rds",
    "labor.rds"
  )
) -> labor

# endregion
# exports
# region: exports
box::export(labor)

# endregion
