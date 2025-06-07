# setup
# region: modular
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / roadmap / path / functions / cost[...],
  mod / roadmap / path / data / similarity[...],
  mod / roadmap / path / functions / restart[...],
  mod / roadmap / path / functions / prob[...],
  mod / roadmap / path / functions / employment[...],
  req = mod / roadmap / path / data / req,
  lab = mod / roadmap / path / data / labor,
  mod / utils / data[sublist],
  stats[na.omit],
  gr = igraph,
  readr[...],
  dplyr[...],
  tidyr[...],
)

# endregion
# model
# # grid
# region: career grid
career.grid <- function(xmin, tmin, xmax = NULL, tmax = req$education$doctorate) {
  # assert args in main function
  # generate valid career progressions on a 2d experience vs education grid
  return(
    expand.grid(
      x = req$experience |> sublist(function(x) ifelse(!length(xmax), x >= xmin, x >= xmin & x <= xmax)) |> as.numeric(),
      t = req$education |> sublist(function(t) (t >= tmin) & (t <= tmax)) |> as.numeric()
    )
  )
}

# endregion
# region: basic education
career.grid(0, 0, req$education$high.school, xmax = 0) -> basic.education

# endregion
# region: career progressions
career.grid |>
  Map(
    req$career.req$x,
    req$career.req$t
  ) |>
  c(
    list(basic.education)
  ) ->
career.grids

# endregion
# # vertices
# region: vertices
career.grids |>
  bind_rows(
    .id = "occupation"
  ) |>
  mutate(
    .before = 1,
    occupation = as.integer(occupation),
    vertex = row_number()
  ) ->
vertices

# endregion
# region: vertices prob
vertices |>
  inner_join(
    req$onet.bin$x |>
      select(
        x = from,
        occupation = id,
        x.pct = pct
      ),
    relationship = "many-to-many"
  ) |>
  inner_join(
    req$onet.bin$t |>
      select(
        t = to,
        occupation = id,
        t.pct = pct
      ),
    relationship = "many-to-many"
  ) ->
vertices

# endregion
# exports
# region: exports
vertices |>
  saveRDS(
    getOption("atlas.mod") |>
      file.path(
        "roadmap",
        "path",
        "data",
        "rds",
        "vertices.rds"
      )
  )

# endregion
