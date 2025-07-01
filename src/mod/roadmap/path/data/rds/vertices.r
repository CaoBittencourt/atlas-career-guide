# setup
# region: modular
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  assert = utils / assert,
  roadmap / path / functions / cost[...],
  roadmap / path / functions / restart[...],
  roadmap / path / functions / prob[...],
  roadmap / path / functions / employment[...],
  req = roadmap / path / data / req,
  lab = roadmap / path / data / labor,
  pro = utils / probs,
  roadmap / path / data / similarity[...],
  utils / data[sublist],
  stats[na.omit],
  gr = igraph,
  readr[...],
  dplyr[...],
  tidyr[...],
)

# endregion
# model
# # grid
# region: career progression grid
# career progressions and similarity
mtx_similarity |>
  rename(
    careerTo = 1
  ) |>
  pivot_longer(
    cols = -1,
    names_to = 'career',
    values_to = 'similarity'
  ) |>
  filter(
    career != careerTo
  ) |>
  inner_join(
    req$df_ids |>
      select(
        -id_soc_code
      ) |>
      rename(
        careerTo = occupation,
        idTo = id
      ),
  ) |>
  inner_join(
    req$df_ids |>
      select(
        -id_soc_code
      ) |>
      rename(
        career = occupation,
        id = id
      ),
  ) |>
  select(
    -starts_with('career')
  ) |>
  rename_with(
    ~ gsub(
      'id',
      'career',
      .x
    )
  ) |>
  relocate(
    career,
    careerTo
  ) -> careerGrid

# career progressions and similarity
careerGrid |>
  inner_join(
    lab$labor |>
      select(-occupation) |>
      mutate(
        wTotal = sum(w, na.rm = T)
      ),
    by = c('careerTo' = 'id'),
    multiple = 'all'
  ) |>
  mutate(
    prob = (w / wTotal) * similarity * (similarity >= 0.5)
  ) |> 
  filter(
    prob > 0
  ) -> careerGrid

# endregion

# region: vertex grid
career.grid <- function(
  xmin,
  tmin,
  xmax = NULL,
  tmax = req$education$doctorate
) {
  # assert args in main function
  # generate valid career progressions on a 2d experience vs education grid
  return(
    expand.grid(
      x = req$experience |>
        sublist(function(x) {
          ifelse(!length(xmax), x >= xmin, x >= xmin & x <= xmax)
        }) |>
        as.numeric(),
      t = req$education |>
        sublist(function(t) (t >= tmin) & (t <= tmax)) |>
        as.numeric()
    )
  )
}

# endregion
# region: basic education
req$df_ids |>
  filter(occupation == "Basic Education") |>
  pull(id) -> basic.education.id

career.grid(
  xmin = req$onet.bin$x |> filter(id == basic.education.id) |> pull(from),
  tmin = req$onet.bin$t |> filter(id == basic.education.id) |> pull(from),
  xmax = req$onet.bin$x |> filter(id == basic.education.id) |> pull(to),
  tmax = req$onet.bin$t |> filter(id == basic.education.id) |> pull(to)
) -> basic.education

basic.education |>
  mutate(
    .before = 1,
    occupation = basic.education.id
  ) -> basic.education

# endregion
# region: career progressions
career.grid |>
  Map(
    req$career.req |> filter(id != basic.education.id) |> pull(x),
    req$career.req |> filter(id != basic.education.id) |> pull(t)
  ) -> career.grids

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
  ) -> vertices

# endregion
# region: vertices indie prob
vertices |>
  left_join(
    req$onet.bin$x |>
      select(
        x = from,
        x.ub = to,
        occupation = id,
        x.pct = pct
      ),
    relationship = "many-to-many"
  ) |>
  left_join(
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
    x.lb = x,
    x.ub,
    t.lb = t,
    t.ub
  ) -> vertices

# endregion
# region: conditional prob dist
# conditional probability distribution
# of education given experience level
list(
  "norm" = function(x, t) {
    return(
      t |> dnorm(x, t)
    )
  }
) -> pdf.t_x

# endregion
# region: vertices joint prob
vertices |>
  filter(
    occupation != basic.education.id
  ) |>
  group_by(occupation) |>
  mutate(
    const = pdf.t_x[[1]] |>
      pro$norm.const(
        pmin(x.lb),
        pmax(x.ub),
        pmin(t.lb),
        pmax(t.ub)
      )
  ) |>
  ungroup() |>
  mutate(
    prob = x.pct *
      t.pct *
      pro$prob.y_x(
        pdf.t_x[[1]],
        x.lb,
        x.ub,
        t.lb,
        t.ub
      ) /
      const
  ) |>
  select(
    occupation,
    vertex,
    x = x.lb,
    t = t.lb,
    prob
  ) |>
  bind_rows(
    basic.education |>
      mutate(
        vertex = 1L + max(vertices$vertex),
        prob = 1
      )
  ) -> vertices

# - E[U] = Pr[v2 | v1] * u(v2) = ((w(v2) / w) * s(v1, v2) * [s(v1, v2) >= 0.5]) * u(v2) >= 0
#         - cost(v1,v2)
#         - weight := cost * ((1 - E[u]) ^ !is.infinity(cost))

# endregion
# exports
# region: exports
vertices |>
  saveRDS(
    Sys.getenv("ATLAS_MOD") |>
      file.path(
        "roadmap",
        "path",
        "data",
        "rds",
        "vertices.rds"
      )
  )

# endregion
