# data
Sys.getenv("ATLAS_MOD") |>
  file.path(
    "roadmap",
    "path",
    "data",
    "rds",
    "paths.rds"
  ) -> paths.path

if (!file.exists(paths.path)) {
  warning('Career grid not found. Attempting to create new grid.')
}

if (!file.exists(paths.path)) {
  Sys.getenv("ATLAS_MOD") |>
    file.path(
      "roadmap",
      "path",
      "data",
      "grid.r"
    ) -> grid.path

  if (file.exists(grid.path)) {
    source(grid.path)
  } else {
    warning('Could not create new grid: "grid.r" not found.')
  }
}

paths.path |> readRDS() -> paths

# exports
box::export(paths)
