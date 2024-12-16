# imports
box::use(stringr[str_subset])
modular::project.options("atlas")

# run all test files
getOption("atlas.root") |>
  file.path("tests") |>
  list.files(full.names = T) |>
  str_subset(".r$") |>
  str_subset("tests.r$", negate = T) |>
  lapply(function(file) {
    source(file)$value
  }) |>
  unlist()
