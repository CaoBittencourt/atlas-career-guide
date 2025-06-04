# setup roadmap module data
modular::project.options("atlas")

source(getOption("atlas.mod") |> file.path(file.path("roadmap", "path", "data", "rds", "req.r")))
source(getOption("atlas.mod") |> file.path(file.path("roadmap", "path", "data", "rds", "graph.r")))
source(getOption("atlas.mod") |> file.path(file.path("roadmap", "path", "data", "rds", "base_cost.r")))
