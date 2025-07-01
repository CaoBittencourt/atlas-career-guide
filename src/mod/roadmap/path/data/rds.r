# setup roadmap module data
options(box.path = Sys.getenv("ATLAS_MOD"))

source(
  getOption("atlas.mod") |>
    file.path(file.path("roadmap", "path", "data", "rds", "req.r"))
)

source(
  getOption("atlas.mod") |>
    file.path(file.path("roadmap", "path", "data", "rds", "labor.r"))
)

source(
  getOption("atlas.mod") |>
    file.path(file.path("roadmap", "path", "data", "rds", "vertices.r"))
)

source(
  getOption("atlas.mod") |>
    file.path(file.path("roadmap", "path", "data", "rds", "graph.r"))
)

source(
  getOption("atlas.mod") |>
    file.path(file.path("roadmap", "path", "data", "rds", "base_cost.r"))
)
