# setup
# region: imports
box::use(
  assert = utils / assert,
  roadmap / path / data / vertices[...],
  dplyr[...]
)

# endregion
# non vectorized
# region: match.vertex non vectorized function
match.vertex_ <- function(occupation.from, x.from = 0, t.from = 0) {
  # assert args in main
  # dispatch
  return(
    vertices |>
      filter(
        occupation == occupation.from
      ) |>
      arrange(
        sqrt(
          (x - x.from)^2 +
            (t - t.from)^2
        )
      ) |>
      slice(1) |>
      pull(vertex)
  )
}

# endregion
# dispatch
# region: match.vertex generic function
match.vertex_ <- Vectorize(match.vertex_, vectorize.args = "occupation.from")

match.vertex <- function(occupation.from, x.from = 0, t.from = 0) {
  # assert args
  stopifnot(round(occupation.from) == occupation.from)
  assert$base$validate.numeric.bounded(x.from, "x.from", F, 0)
  assert$base$validate.numeric.bounded(t.from, "t.from", F, 0)

  # dispatch
  return(match.vertex_(occupation.from, x.from, t.from))
  #   vertices |>
  #     filter(
  #       occupation == occupation.from
  #     ) |>
  #     arrange(
  #       sqrt(
  #         (x - x.from)^2 +
  #           (t - t.from)^2
  #       )
  #     ) |>
  #     slice(1) |>
  #     pull(vertex)
  # )
}

# endregion
# exports
# region: exports
box::export(match.vertex)

# endregion
