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
  ss = compare / seq,
  pro = utils / probs,
  roadmap / path / data / similarity[...],
  utils / data[sublist],
  stats[na.omit],
  gr = igraph,
  arrow[write_parquet],
  readr[...],
  dplyr[...],
  tidyr[...],
)

# endregion
# model
# # grid
# region: vertex grid
vertex.grid <- function(
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

vertex.grid(
  xmin = req$onet.bin$x |> filter(id == basic.education.id) |> pull(from),
  tmin = req$onet.bin$t |> filter(id == basic.education.id) |> pull(from),
  xmax = req$onet.bin$x |> filter(id == basic.education.id) |> pull(to),
  tmax = req$onet.bin$t |> filter(id == basic.education.id) |> pull(to)
) -> basic.education

basic.education |>
  mutate(
    .before = 1,
    career = basic.education.id
  ) -> basic.education

# endregion
# region: career grid
# career and similarity
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
  mutate(
    ß = ss$ss(similarity)
  ) |>
  filter(
    ß > 0.5
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
        wTotal = sum(w, na.rm = T),
        wTilde = (w / wTotal)
      ),
    by = c('careerTo' = 'id'),
    multiple = 'all'
  ) |>
  filter(
    careerTo != basic.education.id
  ) |>
  mutate(
    prob = ß * wTilde
  ) |>
  filter(
    prob > 0
  ) |>
  group_by(career) |>
  mutate(
    prob = prob / sum(prob)
  ) |>
  ungroup() -> careerGrid

# endregion
# region: vertex grid
vertex.grid |>
  Map(
    req$career.req |> filter(id != basic.education.id) |> pull(x),
    req$career.req |> filter(id != basic.education.id) |> pull(t)
  ) -> vertexGrids

# endregion
# # vertices
# region: vertices
vertexGrids |>
  bind_rows(
    .id = "career"
  ) |>
  mutate(
    .before = 1,
    career = as.integer(career),
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
        career = id,
        x.pct = pct
      ),
    relationship = "many-to-many"
  ) |>
  left_join(
    req$onet.bin$t |>
      select(
        t = from,
        t.ub = to,
        career = id,
        t.pct = pct
      ),
    relationship = "many-to-many"
  ) |>
  relocate(
    vertex,
    career,
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
    career != basic.education.id
  ) |>
  group_by(career) |>
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
    career,
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

# endregion
# exports
# region: rds
careerGrid |>
  saveRDS(
    Sys.getenv("ATLAS_MOD") |>
      file.path(
        "roadmap",
        "path",
        "data",
        "rds",
        "careers.rds"
      )
  )

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
# region: parquet
careerGrid |>
  write_parquet(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path(
        "parquet",
        "careers.parquet"
      )
  )

vertices |>
  write_parquet(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path(
        "parquet",
        "vertices.parquet"
      )
  )

# endregion
