# setup
# region: data
readRDS(
  file.path(
    getOption("atlas.mod"),
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
