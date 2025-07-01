# setup
# region: data
readRDS(
  file.path(
    getOption("atlas.mod"),
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
