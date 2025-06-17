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
  mod / roadmap / path / data / pdf[...],
  pro = mod / utils / probs,
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
# region: vertices indie prob
vertices |>
  inner_join(
    req$onet.bin$x |>
      select(
        x = from,
        x.ub = to,
        occupation = id,
        x.pct = pct
      ),
    relationship = "many-to-many"
  ) |>
  inner_join(
    req$onet.bin$t |>
      select(
        t = from,
        t.ub = to,
        occupation = id,
        t.pct = pct
      ),
    relationship = "many-to-many"
  ) |>
  relocate(
    vertex,
    occupation,
    x.lb = x, x.ub,
    t.lb = t, t.ub
  ) ->
vertices

# endregion
# region: vertices joint prob
vertices |>
  filter(
    occupation == c(1, 2)
  ) ->
dsds

dsds |>
  group_by(occupation) |>
  # group_by(occupation, x.lb) |>
  mutate(
    const =
      pdf.t_x$norm |>
        pro$norm.const(
          pmin(x.lb), pmax(x.ub),
          pmin(t.lb), pmax(t.ub)
        )
  ) |>
  ungroup() |>
  print(
    n = Inf
  ) |>
  mutate(
    prob = x.pct *
      pro$prob.y_x(
        pdf.t_x$norm,
        x.lb, x.ub,
        t.lb, t.ub
      ) / const
  ) |>
  group_by(occupation) |>
  reframe(
    prob = sum(prob)
  )

vertices |>
  mutate(
    prob =
      x.pct *
        pro$prob.y_x(
          pdf.t_x$norm,
          x.lb, x.ub,
          t.lb, t.ub
        ) / pro$norm.const(
          pdf.t_x$norm,
          x.lb, x.ub,
          t.lb, t.ub
        )
  ) |>
  select(
    occupation,
    vertex,
    x.lb, x.ub,
    t.lb, t.ub,
    prob
  ) ->
dsds

dsds |>
  filter(
    occupation == c(1, 2)
  ) |>
  group_by(occupation) |>
  reframe(
    prob = sum(prob)
  )

vertices

# - E[U] = Pr[v2 | v1] * u(v2) = ((w(v2) / w) * s(v1, v2) * [s(v1, v2) >= 0.5]) * u(v2) >= 0
#         - cost(v1,v2)
#         - weight := cost * ((1 - E[u]) ^ !is.infinity(cost))

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
