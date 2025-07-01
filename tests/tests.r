# imports
box::use(stringr[str_subset])
options(box.path = Sys.getenv("ATLAS_MOD"))

# run all test files
Sys.getenv("ATLAS_ROOT") |>
  file.path("tests", "test-files") |>
  list.files(full.names = T) |>
  str_subset(".r$") |>
  lapply(function(file) {
    source(file)$value
  }) |>
  unlist()
