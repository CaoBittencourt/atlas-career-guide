# setup
# region: data
readRDS(
  file.path(
    getOption("atlas.mod"),
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
