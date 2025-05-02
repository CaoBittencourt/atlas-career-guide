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

# endregion
# exports
# region: exports
box::export(
  education,
  experience,
  career.req
)

# endregion
