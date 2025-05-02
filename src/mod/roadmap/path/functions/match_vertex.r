# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / roadmap / path / data / vertices[...],
  dplyr[...]
)

# endregion
# dispatch
# region: match.vertex generic function
match.vertex <- function(occupation.from, x.from, t.from) {
  # assert args
  stopifnot(round(occupation.from) == occupation.from)
  assert$base$validate.numeric.bounded(x.from, "x.from", F, 0)
  assert$base$validate.numeric.bounded(t.from, "t.from", F, 0)

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
# exports
# region: exports
box::export(match.vertex)

# endregion
