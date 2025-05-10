# setup roadmap module data
modular::project.options("atlas")

c(
  getOption("atlas.mod") |> file.path(file.path("roadmap", "path", "data", "rds", "req.r")),
  getOption("atlas.mod") |> file.path(file.path("roadmap", "path", "data", "rds", "similarity.r")),
  getOption("atlas.mod") |> file.path(file.path("roadmap", "path", "data", "rds", "graph.r")),
  getOption("atlas.mod") |> file.path(file.path("roadmap", "path", "data", "rds", "base_cost.r"))
) |>
  lapply(source)
