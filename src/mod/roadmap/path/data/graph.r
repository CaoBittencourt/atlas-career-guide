# setup
# region: data
readRDS(
  file.path(
    Sys.getenv("ATLAS_MOD"),
    "roadmap",
    "path",
    "data",
    "rds",
    "paths.rds"
  )
) -> paths

# endregion
# exports
# region: exports
box::export(paths)

# endregion
