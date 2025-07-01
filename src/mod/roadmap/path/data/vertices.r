# setup
# region: data
readRDS(
  file.path(
    Sys.getenv("ATLAS_MOD"),
    "roadmap",
    "path",
    "data",
    "rds",
    "vertices.rds"
  )
) -> vertices

# endregion
# exports
# region: exports
box::export(vertices)

# endregion
