# setup
# region: data
load(
  file.path(
    Sys.getenv("ATLAS_MOD"),
    "roadmap",
    "path",
    "data",
    "rds",
    "req.rdata"
  )
)

# endregion
# exports
# region: exports
box::export(
  df_ids,
  experience,
  education,
  career.req,
  onet.bin
)

# endregion
