# setup
# region: imports
box::use(
  req = mod / roadmap / path / data / req,
  readr[...],
  dplyr[...],
  tidyr[...],
)

# endregion
# data
# region: similarity matrix
req$career.req |>
  select(
    to = occupation
  ) |>
  inner_join(
    "atlas.root" |>
      getOption() |>
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
box::export(mtx_similarity)

# endregion
