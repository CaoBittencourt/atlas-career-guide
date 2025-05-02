# setup
# region: data
readRDS(
  file.path(
    getOption("atlas.mod"),
    "roadmap",
    "path",
    "data",
    "rds",
    "similarity.rds"
  )
) -> mtx_similarity

# endregion
# exports
# region: exports
box::export(mtx_similarity)

# endregion
