# setup
# region: modular
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  readr[...],
  dplyr[...],
  tidyr[...],
  req = mod / roadmap / path / data / req,
)

# endregion
# data
# region: similarity matrix
req$career.req |>
  select(
    to = occupation
  ) |>
  inner_join(
    getOption("atlas.root") |>
      file.path(
        "articles",
        "1.introduction-matching",
        "output",
        "similarity_matrix.csv"
      ) |>
      read_csv() |>
      rename(
        to = 1
      ) |>
      relocate(
        req$career.req$
          occupation
      )
  ) |>
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~.x
      # .fns = ~ .x^2
    )
  ) ->
mtx_similarity

# endregion
# exports
# region: exports
mtx_similarity |>
  saveRDS(
    getOption("atlas.mod") |>
      file.path(
        "roadmap",
        "path",
        "data",
        "rds",
        "similarity.rds"
      )
  )

# endregion
