# setup
# region: data
load(
  file.path(
    getOption("atlas.mod"),
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
