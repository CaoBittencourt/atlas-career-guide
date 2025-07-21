# setup
# region: data
list(
  careers = readRDS(
    file.path(
      Sys.getenv("ATLAS_MOD"),
      "roadmap",
      "path",
      "data",
      "rds",
      "careers.rds"
    )
  ),

  vertices = readRDS(
    file.path(
      Sys.getenv("ATLAS_MOD"),
      "roadmap",
      "path",
      "data",
      "rds",
      "vertices.rds"
    )
  )
) -> detailed

list(
  careers = readRDS(
    file.path(
      Sys.getenv("ATLAS_MOD"),
      "roadmap",
      "path",
      "data",
      "rds",
      "careers_expected.rds"
    )
  ),

  vertices = readRDS(
    file.path(
      Sys.getenv("ATLAS_MOD"),
      "roadmap",
      "path",
      "data",
      "rds",
      "vertices_expected.rds"
    )
  )
) -> expected

# endregion
# exports
# region: exports
box::export(detailed, expected)

# endregion
