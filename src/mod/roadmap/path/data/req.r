# setup
# region: data
readRDS(
  file.path(
    getOption("atlas.mod"),
    "roadmap",
    "path",
    "data",
    "rds",
    "education.rds"
  )
) -> education

readRDS(
  file.path(
    getOption("atlas.mod"),
    "roadmap",
    "path",
    "data",
    "rds",
    "experience.rds"
  )
) -> experience

readRDS(
  file.path(
    getOption("atlas.mod"),
    "roadmap",
    "path",
    "data",
    "rds",
    "career_req.rds"
  )
) -> career.req

readRDS(
  file.path(
    getOption("atlas.mod"),
    "roadmap",
    "path",
    "data",
    "rds",
    "onet_bin.rds"
  )
) -> onet.bin

# endregion
# exports
# region: exports
box::export(
  education,
  experience,
  career.req,
  onet.bin
)

# endregion
