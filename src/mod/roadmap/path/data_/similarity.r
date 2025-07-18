# setup
# region: data
readRDS(
  file.path(
    Sys.getenv("ATLAS_MOD"),
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
