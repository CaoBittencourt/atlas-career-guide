# setup
# region: modules
options(box.path = Sys.getenv('ATLAS_MOD'))

# endregion
# region: imports
box::use(
  assert = utils / assert,
  roadmap / path / functions / cost[...],
  roadmap / path / functions / restart[...],
  roadmap / path / functions / prob[...],
  roadmap / path / functions / employment[...],
  dt = dtplyr,
  utils / data[sublist],
  stats[na.omit],
  readr[...],
  dplyr[...],
  tidyr[...],
  arrow[write_parquet],
)

# endregion
# region: data
box::use(
  roadmap / path / data / similarity[...],
  req = roadmap / path / data / req,
  lab = roadmap / path / data / labor,
  roadmap / path / data / vertices[...],
)

# endregion
# model
# region: P(careerTo | career)
careers |>
  select(
    career,
    careerTo,
    similarity,
    prob
  ) -> careers

# endregion
# region: movement cost function
# optimize cost and career path transition
# assuming morphing hypothesis
# relax morphing with mcts later
move.cost <- function(ßkq, xk, xq, tk, tq) {
  # assert args in main function

  ßkq.eq <- ßkq * (ßkq >= 0.5)
  # equivalent similarity

  pmax(xq - xk * ßkq.eq, 0) / ßkq -> req.x
  # experience gap

  pmax(tq - tk * ßkq.eq, 0) / ßkq -> req.t
  # education gap

  pmin(xq, req.x) -> req.x
  pmin(tq, req.t) -> req.t
  # assume one must have all equivalent years
  # before attempting to switch careers
  # assume order of study and work doesn't matter
  # assume reset cost is zero
  # movement types:
  # - restart x, recycle t
  # - restart t, recycle x
  # - restart t, restart x
  # - recycle t, recycle x

  return(list(
    work = req.x,
    study = req.t,
    total = req.x + req.t
  ))
}

# move.cost(0.4, 3, 5, 4, 4)
# move.cost(0.8, 3, 5, 4, 4)
# move.cost(0, 3, 5, 4, 4)
# move.cost(0, 1000, 5, 1000, 4)

# endregion
# region: example
# k <- 1
# vertices |> filter(career == k) |> slice_sample(n = 1, weight_by = prob) -> vk
# careers |> filter(career == k) |>  slice_sample(n = 1, weight_by = prob) -> kq
# vertices |> filter(career == kq$careerTo) |> slice_sample(n = 1, weight_by = prob) -> vq
# k
# vk
# kq
# vq
# move.cost(kq$similarity, vk$x, vq$x, vk$t, vq$t)

# endregion
# exports
# region: parquets
careers |>
  mutate(
    across(
      .cols = starts_with('career'),
      .fns = as.integer
    )
  ) |>
  group_by(career) |>
  mutate(
    prob = prob / sum(prob)
  ) |>
  ungroup() |>
  write_parquet(
    Sys.getenv("ATLAS_DATA") |>
      file.path(
        'road',
        "careers.parquet"
      )
  )

vertices |>
  mutate(
    across(
      .cols = starts_with('career'),
      .fns = as.integer
    )
  ) |>
  group_by(career) |>
  mutate(
    prob = prob / sum(prob)
  ) |>
  ungroup() |>
  write_parquet(
    Sys.getenv("ATLAS_DATA") |>
      file.path(
        'road',
        "vertices.parquet"
      )
  )

# endregion
