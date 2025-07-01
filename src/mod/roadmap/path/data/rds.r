# setup roadmap module data
options(box.path = Sys.getenv("ATLAS_MOD"))

source(
  Sys.getenv("ATLAS_MOD") |>
    file.path(file.path("roadmap", "path", "data", "rds", "req.r"))
)

source(
  Sys.getenv("ATLAS_MOD") |>
    file.path(file.path("roadmap", "path", "data", "rds", "labor.r"))
)

source(
  Sys.getenv("ATLAS_MOD") |>
    file.path(file.path("roadmap", "path", "data", "rds", "vertices.r"))
)

source(
  Sys.getenv("ATLAS_MOD") |>
    file.path(file.path("roadmap", "path", "data", "rds", "graph.r"))
)

source(
  Sys.getenv("ATLAS_MOD") |>
    file.path(file.path("roadmap", "path", "data", "rds", "base_cost.r"))
)
