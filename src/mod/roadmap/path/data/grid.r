# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  assert = utils / assert,

  dplyr[...],
  tidyr[...],
  stringr[...],

  readr[...],
  readxl[read_excel],
  arrow[write_parquet],

  utils / bin[...],
  ss = compare / seq,
  pro = utils / probs,
  utils / data[sublist],
  stats[setNames, weighted.mean, na.omit, dnorm],

  roadmap / path / functions / cost[...],
  roadmap / path / functions / employment[...],
  roadmap / path / functions / prob[...],
  roadmap / path / functions / restart[...],

  gr = igraph,
)

# endregion
# data
# # requirements
# region: required education
list(
  high.school = 17 - 17,
  associate = 19 - 17,
  bachelor = 21 - 17,
  master = 23 - 17,
  doctorate = 28 - 17
) -> education

tmax <- 20

# endregion
# region: required experience
list(
  intern = 0,
  junior = 1,
  # junior = 2,
  associate = 3,
  mid.level = 5,
  senior = 10
) -> experience

xmax <- 60

# endregion
# region: onet data
# ids
Sys.getenv("ATLAS_OLD_DATA") |>
  read.csv() |>
  select(
    id_soc_code,
    occupation
  ) |>
  arrange(
    occupation
  ) |>
  mutate(
    .before = 1,
    id = row_number()
  ) -> df_ids

# requirements
Sys.getenv("ATLAS_DATA") |>
  file.path(
    "db_27_3_excel",
    "Education, Training, and Experience.xlsx"
  ) |>
  read_excel() |>
  dplyr::filter(
    `Scale ID` %in% c("RW", "RL")
  ) -> df_req

# note: (Category, `O*NET-SOC Code`, `Scale ID`) are the composite primary key
# all(df_req |> group_by(Category, `O*NET-SOC Code`, `Scale ID`) |> tally() |> pull(n) == 1)

Sys.getenv("ATLAS_DATA") |>
  file.path(
    "db_27_3_excel",
    "Education, Training, and Experience Categories.xlsx"
  ) |>
  read_excel() |>
  dplyr::filter(
    `Scale ID` %in% c("RW", "RL")
  ) -> df_req_cat

tribble(
  ~`Scale ID`,
  ~`Category`,
  ~`Category Description`,
  ~`years`,
  "RL",
  1,
  "Less than a High School Diploma",
  education$high.school,
  "RL",
  2,
  "High School Diploma - or the equivalent (for example, GED)",
  education$high.school,
  "RL",
  3,
  "Post-Secondary Certificate - awarded for training completed after high school (for example, in agriculture or natural resources, computer services, personal or culinary services, engineering technologies, healthcare, construction trades, mechanic and repair technologies, or precision production)",
  education$associate,
  "RL",
  4,
  "Some College Courses",
  education$associate,
  "RL",
  5,
  "Associate's Degree (or other 2-year degree)",
  education$associate,
  "RL",
  6,
  "Bachelor's Degree",
  education$bachelor,
  "RL",
  7,
  "Post-Baccalaureate Certificate - awarded for completion of an organized program of study; designed for people who have completed a Baccalaureate degree but do not meet the requirements of academic degrees carrying the title of Master.",
  education$master,
  "RL",
  8,
  "Master's Degree",
  education$master,
  "RL",
  9,
  "Post-Master's Certificate - awarded for completion of an organized program of study; designed for people who have completed a Master's degree but do not meet the requirements of academic degrees at the doctoral level.",
  education$doctorate,
  "RL",
  10,
  "First Professional Degree - awarded for completion of a program that: requires at least 2 years of college work before entrance into the program, includes a total of at least 6 academic years of work to complete, and provides all remaining academic requirements to begin practice in a profession.",
  education$doctorate,
  "RL",
  11,
  "Doctoral Degree",
  education$doctorate,
  "RL",
  12,
  "Post-Doctoral Training",
  education$doctorate,
  "RW",
  1,
  "None",
  experience$intern,
  "RW",
  2,
  "Up to and including 1 month",
  1 / 12,
  "RW",
  3,
  "Over 1 month, up to and including 3 months",
  mean(c(1, 3)) / 12,
  "RW",
  4,
  "Over 3 months, up to and including 6 months",
  mean(c(3, 6)) / 12,
  "RW",
  5,
  "Over 6 months, up to and including 1 year",
  mean(c(6, 12)) / 12,
  "RW",
  6,
  "Over 1 year, up to and including 2 years",
  mean(c(1, 2)),
  "RW",
  7,
  "Over 2 years, up to and including 4 years",
  mean(c(2, 4)),
  "RW",
  8,
  "Over 4 years, up to and including 6 years",
  mean(c(4, 6)),
  "RW",
  9,
  "Over 6 years, up to and including 8 years",
  mean(c(6, 8)),
  "RW",
  10,
  "Over 8 years, up to and including 10 years",
  mean(c(8, 10)),
  "RW",
  11,
  "Over 10 years",
  mean(c(10, 30)),
) -> df_req_cat

# endregion
# region: data wrangling
df_req |>
  mutate(
    .before = 1,
    id_soc_code = `O*NET-SOC Code` |>
      str_sub(1, -4)
  ) |>
  mutate(
    pct = `Data Value` / 100
  ) |>
  inner_join(
    df_req_cat,
    relationship = "many-to-many"
  ) |>
  select(
    scaleId = `Scale ID`,
    id_soc_code,
    onet_soc_code = `O*NET-SOC Code`,
    pct,
    years
  ) -> onet_req

# endregion
# region: similarity matrix
df_ids |>
  select(
    to = occupation
  ) |>
  inner_join(
    Sys.getenv("ATLAS_RDS") |>
      file.path(
        "similarity.rds"
      ) |>
      readRDS() |>
      rename(
        to = 1
      ) |>
      relocate(
        df_ids$occupation
      )
  ) |>
  as_tibble() |>
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~.x
    )
  ) -> df_similarity

# endregion
# region: closest matches
# get closest match
df_similarity |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "similarity"
  ) |>
  inner_join(
    df_ids |> rename_with(.fn = ~ .x |> paste0(".to")),
    by = c(
      "to" = "occupation.to"
    )
  ) |>
  group_by(to) |>
  filter(
    id_soc_code.to %in% df_ids$id_soc_code
  ) |>
  arrange(
    -similarity,
    .by_group = T
  ) |>
  slice(-1) |>
  slice(1) |>
  ungroup() |>
  inner_join(
    df_ids |> rename_with(.fn = ~ .x |> paste0(".from")),
    by = c(
      "from" = "occupation.from"
    )
  ) -> df_closest_match

all(
  df_closest_match |>
    filter(
      id.from %in%
        (df_ids |>
          filter(
            !(id_soc_code %in% df_ids$id_soc_code)
          ) |>
          pull(id))
    ) |>
    pull(id_soc_code.to) %in%
    df_ids$id_soc_code
)

onet_req |>
  inner_join(
    df_closest_match |>
      select(
        id.replace = id.from,
        id = id.to,
        id_soc_code.replace = id_soc_code.from,
        id_soc_code = id_soc_code.to
      ) |>
      inner_join(
        df_ids
      ) |>
      transmute(
        id = id,
        occupation = occupation,
        id_soc_code = if_else(
          !(id_soc_code %in%
            onet_req$id_soc_code),
          id_soc_code.replace,
          id_soc_code
        )
      ),
    relationship = "many-to-many"
  ) |>
  arrange(
    id,
    scaleId,
    years
  ) -> onet_req

# endregion
# region: onet binned data => standard numeric requirement bins
Map(
  function(df, bins) {
    df |>
      group_by(id) |>
      reframe(
        years |>
          bin(
            bins,
            pct
          )
      )
  },
  df = onet_req |>
    group_by(scaleId) |>
    group_split(),
  bins = list(
    as.numeric(education),
    as.numeric(experience)
  )
) -> onet.bin

onet.bin |> names() <- c("t", "x")

onet.bin$t |>
  inner_join(
    tibble(
      binId = education |> seq_along(),
      type = education |> names()
    )
  ) |>
  mutate(
    to = replace_na(to, tmax)
  ) -> onet.bin$t

onet.bin$x |>
  inner_join(
    tibble(
      binId = experience |> seq_along(),
      type = experience |> names()
    )
  ) |>
  mutate(
    to = replace_na(to, xmax)
  ) -> onet.bin$x

# endregion
# region: basic education
basic.education.id <- max(df_ids$id) + 1

df_ids |>
  bind_rows(
    tibble(
      id = basic.education.id,
      occupation = "Basic Education"
    )
  ) -> df_ids

onet.bin$t |>
  bind_rows(
    tibble(
      id = basic.education.id,
      binId = 1,
      from = 0,
      to = education$high.school,
      pct = 1,
      type = "high.school"
    )
  ) -> onet.bin$t

onet.bin$x |>
  bind_rows(
    tibble(
      id = basic.education.id,
      binId = 1,
      from = 0,
      to = 0,
      pct = 1,
      type = "intern"
    )
  ) -> onet.bin$x

# endregion
# region: similarity matrix
df_similarity |>
  mutate(
    # hypothesis:
    # basic education has, say, 80%
    # similarity with all occupations
    `Basic Education` = 0.8
  ) -> df_similarity

df_similarity |>
  bind_rows(
    c(
      "Basic Education",
      # all occupations have perfect similiarty
      # with basic education (i.e. cost to reset is zero)
      rep(1, 874) |> as.list()
    ) |>
      setNames(
        df_similarity |>
          names()
      ) |>
      as_tibble()
  ) -> df_similarity

# endregion
# region: requirements data frame
onet.bin |>
  bind_rows(
    .id = "var"
  ) |>
  group_by(id) |>
  filter(
    pct > 0
  ) |>
  group_by(id, var) |>
  reframe(
    from = min(from)
  ) |>
  pivot_wider(
    id_cols = id,
    names_from = var,
    values_from = from
  ) |>
  inner_join(
    df_ids
  ) -> career.req

# endregion
# # labor statistics
# region: labor data
# ids
df_ids |>
  left_join(
    # labor statistics
    Sys.getenv("ATLAS_LABOR") |>
      readRDS()
  ) |>
  select(
    id,
    occupation,
    w = employment_variants,
    wage
  ) -> df_labor

# endregion
# # grid
# region: vertex grid
vertex.grid <- function(
  xmin,
  tmin,
  xmax = NULL,
  tmax = education$doctorate
) {
  # assert args in main function
  # generate valid career progressions on a 2d experience vs education grid
  return(
    expand.grid(
      x = experience |>
        sublist(function(x) {
          ifelse(!length(xmax), x >= xmin, x >= xmin & x <= xmax)
        }) |>
        as.numeric(),
      t = education |>
        sublist(function(t) (t >= tmin) & (t <= tmax)) |>
        as.numeric()
    )
  )
}

# endregion
# region: basic education
df_ids |>
  filter(occupation == "Basic Education") |>
  pull(id) -> basic.education.id

vertex.grid(
  xmin = onet.bin$x |> filter(id == basic.education.id) |> pull(from),
  tmin = onet.bin$t |> filter(id == basic.education.id) |> pull(from),
  xmax = onet.bin$x |> filter(id == basic.education.id) |> pull(to),
  tmax = onet.bin$t |> filter(id == basic.education.id) |> pull(to)
) -> basic.education

basic.education |>
  mutate(
    .before = 1,
    career = basic.education.id
  ) -> basic.education

# endregion
# region: career grid
# career and similarity
df_similarity |>
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
    df_ids |>
      select(
        -id_soc_code
      ) |>
      rename(
        careerTo = occupation,
        idTo = id
      ),
  ) |>
  inner_join(
    df_ids |>
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
    df_labor |>
      select(-occupation) |>
      mutate(
        wTotal = sum(w, na.rm = T),
        wTilde = (w / wTotal)
      ),
    by = c('careerTo' = 'id'),
    multiple = 'all'
  ) |>
  mutate(
    wTilde = ifelse(
      careerTo == basic.education.id,
      1,
      wTilde
    ),
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
    career.req |> filter(id != basic.education.id) |> pull(x),
    career.req |> filter(id != basic.education.id) |> pull(t)
  ) -> vertexGrids

# endregion
# vertices
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
    onet.bin$x |>
      select(
        x = from,
        x.ub = to,
        career = id,
        x.pct = pct
      ),
    relationship = "many-to-many"
  ) |>
  left_join(
    onet.bin$t |>
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
# region: movement cost
move.cost <- function(ßkq, xk, xq, tk, tq) {
  # assert args in main function

  ßkq.eq <- ßkq * (ßkq >= 0.5)
  # equivalent similarity

  pmax(xq - xk * ßkq.eq, 0) / ßkq -> req.x
  # experience gap

  pmax(tq - tk * ßkq.eq, 0) / ßkq -> req.t
  # education gap

  return(req.x + req.t)
}

# endregion
# model
# region: expected movement cost heuristic
# adjusted requirements
# if similarity of basic education is not 100%
# adjust education and experience requirements
# such that starting from zero (i.e. basic education)
# takes the same amount of time as it did before
df_similarity |>
  select(
    occupation = to,
    basic.edu = `Basic Education`
  ) |>
  inner_join(df_ids) |>
  select(
    career = id,
    basic.edu
  ) -> df_adjust

vertices |>
  inner_join(df_adjust) |>
  mutate(
    x = x * basic.edu,
    t = t * basic.edu
  ) |>
  select(-basic.edu) |>
  group_by(career) |>
  reframe(
    x = stats::weighted.mean(x, prob),
    t = stats::weighted.mean(t, prob)
  ) -> vertices.expected

careerGrid |>
  inner_join(
    vertices.expected,
    by = c('career' = 'career')
  ) |>
  inner_join(
    vertices.expected,
    by = c('careerTo' = 'career'),
    suffix = c('', '.to')
  ) |>
  mutate(
    id = row_number(),
    cost = move.cost(
      ß,
      x,
      x.to,
      t,
      t.to
    )
  ) -> careerGrid.expected

vertices.expected |>
  mutate(
    .after = career,
    vertex = career
  ) -> vertices.expected

careerGrid.expected |>
  mutate(
    .after = careerTo,
    vertex = career,
    vertexTo = careerTo
  ) -> careerGrid.expected

# endregion
# region: career-based expected cost model
careerGrid.expected |>
  select(
    career,
    careerTo
  ) |>
  as.matrix() |>
  gr$graph.edgelist(
    directed = T
  ) |>
  gr$set.edge.attribute(
    "type",
    value = "switch"
  ) |>
  gr$set.edge.attribute(
    "weight",
    value = careerGrid.expected$cost
  ) |>
  gr$set.edge.attribute(
    "cost",
    value = careerGrid.expected$cost
  ) |>
  gr$set.edge.attribute(
    "occupation.from",
    value = careerGrid.expected$career
  ) |>
  gr$set.edge.attribute(
    "occupation.to",
    value = careerGrid.expected$careerTo
  ) |>
  gr$set.edge.attribute(
    "vertex.from",
    value = careerGrid.expected$vertex
  ) |>
  gr$set.edge.attribute(
    "vertex.to",
    value = careerGrid.expected$vertexTo
  ) |>
  gr$set.edge.attribute(
    "x.from",
    value = careerGrid.expected$x
  ) |>
  gr$set.edge.attribute(
    "x.to",
    value = careerGrid.expected$x.to
  ) |>
  gr$set.edge.attribute(
    "t.from",
    value = careerGrid.expected$t
  ) |>
  gr$set.edge.attribute(
    "t.to",
    value = careerGrid.expected$t.to
  ) |>
  gr$set.edge.attribute(
    "table.id",
    value = careerGrid.expected$id
  ) |>
  gr$set.edge.attribute(
    "util",
    value = 1
  ) -> paths.graph.expected

# endregion
# region: movement types
# 1. "teleport" vertically to another occupation at a parallel vertex (same x,t)
# expand.grid(
#   vertex = vertices$vertex,
#   vertex.to = vertices$vertex
# ) ->
# vertices.comb

# vertices.comb |>
#   inner_join(
#     vertices,
#     by = c("vertex.to" = "vertex"),
#     relationship = "many-to-many"
#   ) |>
#   rename_with(
#     .cols = -starts_with("vertex"),
#     .fn = ~ .x |> paste0(".to")
#   ) |>
#   inner_join(
#     vertices,
#     by = c("vertex" = "vertex"),
#     relationship = "many-to-many"
#   ) ->
# paths.switch

# vertices |>
#   inner_join(
#     vertices,
#     suffix = c("", ".to"),
#     by = c("x" = "x", "t" = "t"),
#     relationship = "many-to-many"
#   ) |>
#   mutate(
#     type = "switch"
#   ) -> paths.switch

# 2. increment either experience or education within the same occupation
# with only direct (adjacent) movement, i.e.
# or decrement either experience or education within the same occupation
# with only direct (adjacent) movement and zero cost, i.e.

# valid education movements
# high.school -> associate
# associate -> bachelor
# bachelor -> master
# master -> doctorate

# high.school <- associate
# associate <- bachelor
# bachelor <- master
# master <- doctorate
# education |>
#   unlist() |>
#   as_tibble() |>
#   rename(t = 1) |>
#   # as_tibble(
#   #   rownames = "education"
#   # ) |>
#   # rename(t = 2) |>
#   mutate(
#     t.to = dplyr::lead(t, 1)
#   ) -> education.move

# education.move |>
#   bind_rows(
#     education.move |>
#       rename(
#         t.to = t,
#         t = t.to
#       ) |>
#       na.omit()
#   ) |>
#   mutate(
#     type = "study"
#   ) -> education.move

# valid experience movements
# intern -> junior
# junior -> associate
# associate -> mid.level
# mid.level -> senior

# intern <- junior
# junior <- associate
# associate <- mid.level
# mid.level <- senior
# experience |>
#   unlist() |>
#   as_tibble() |>
#   rename(x = 1) |>
#   # as_tibble(
#   #   rownames = "experience"
#   # ) |>
#   # rename(x = 2) |>
#   mutate(
#     x.to = dplyr::lead(x, 1)
#   ) -> experience.move

# experience.move |>
#   bind_rows(
#     experience.move |>
#       rename(
#         x.to = x,
#         x = x.to
#       ) |>
#       na.omit()
#   ) |>
#   mutate(
#     type = "work"
#   ) -> experience.move

# education move
# vertices |>
#   inner_join(
#     education.move,
#     by = c("t" = "t"),
#     relationship = "many-to-many"
#   ) |>
#   na.omit() -> paths.study

# experience move
# vertices |>
#   inner_join(
#     experience.move,
#     by = c("x" = "x"),
#     relationship = "many-to-many"
#   ) |>
#   na.omit() -> paths.work

# 3. teleport back to basic education (hard reset)
# expand.grid(
#   vertex = vertices |>
#     filter(
#       occupation !=
#         (df_ids |> filter(occupation == "Basic Education") |> pull(id))
#     ) |>
#     pull(vertex),
#   vertex.to = vertices |>
#     filter(
#       occupation ==
#         (df_ids |> filter(occupation == "Basic Education") |> pull(id))
#     ) |>
#     pull(vertex)
# ) |>
#   inner_join(
#     vertices,
#     by = c("vertex" = "vertex"),
#     relationship = "many-to-many"
#   ) |>
#   inner_join(
#     vertices,
#     suffix = c("", ".to"),
#     by = c("vertex.to" = "vertex"),
#     relationship = "many-to-many"
#   ) |>
#   filter(
#     occupation != occupation.to
#   ) |>
#   mutate(
#     type = "reset"
#   ) -> paths.reset

# # 3. teleport to first vertex of any occupation at full (x.to + t.to) cost? (hard reset)
# expand.grid(
#   vertex = vertices$vertex,
#   vertex.to =
#     vertices |>
#       group_by(
#         occupation
#       ) |>
#       slice(1) |>
#       ungroup() |>
#       pull(vertex)
# ) |>
#   inner_join(
#     vertices,
#     by = c("vertex" = "vertex"),
#     relationship = "many-to-many"
#   ) |>
#   inner_join(
#     vertices,
#     suffix = c("", ".to"),
#     by = c("vertex.to" = "vertex"),
#     relationship = "many-to-many"
#   ) |>
#   filter(
#     occupation != occupation.to
#   ) |>
#   mutate(
#     type = "reset"
#   ) ->
# paths.restart

# all valid paths
# bind_rows(
#   # switch careers
#   paths.switch |>
#     mutate(
#       x.to = x,
#       t.to = t
#     ),
#   # reset career
#   paths.reset,
#   # restart career
#   # paths.restart,
#   # move experience
#   paths.work |>
#     inner_join(
#       vertices,
#       suffix = c("", ".to"),
#       by = c(
#         "x.to" = "x",
#         "t" = "t",
#         "occupation" = "occupation"
#       )
#     ) |>
#     mutate(
#       occupation.to = occupation,
#       t.to = t
#     ),
#   # move education
#   paths.study |>
#     inner_join(
#       vertices,
#       suffix = c("", ".to"),
#       by = c(
#         "x" = "x",
#         "t.to" = "t",
#         "occupation" = "occupation"
#       )
#     ) |>
#     mutate(
#       occupation.to = occupation,
#       x.to = x
#     )
# ) |>
#   filter(
#     vertex != vertex.to
#   ) |>
#   select(
#     -ends_with("pct.to")
#   ) |>
#   relocate(
#     starts_with("vertex"),
#     starts_with("occupation"),
#     type,
#     starts_with("x"),
#     starts_with("t")
#   ) -> paths

# endregion
# region: movement similarity
# df_similarity |>
#   pivot_longer(
#     cols = -1,
#     names_to = "from",
#     values_to = "similarity"
#   ) |>
#   inner_join(
#     df_ids |> rename(id.to = id),
#     by = c(
#       "to" = "occupation"
#     )
#   ) |>
#   inner_join(
#     df_ids |> rename(id.from = id),
#     by = c(
#       "from" = "occupation"
#     )
#   ) |>
#   select(
#     occupation = id.from,
#     occupation.to = id.to,
#     similarity
#   ) |>
#   inner_join(
#     paths
#   ) -> paths

# endregion
# region: movement cost
# paths |>
#   mutate(
#     cost = move.cost(
#       skq = similarity,
#       xk = x,
#       xq = x.to,
#       tk = t,
#       tq = t.to
#     )
#   ) -> paths

# endregion
# region: movement payoff
# feasible paths
# paths |>
#   filter(
#     !is.infinite(cost)
#   ) |>
#   mutate(
#     .before = 1,
#     id = row_number()
#   ) |>
#   select(-prob) |>
#   rename(
#     prob = prob.to
#   ) |>
#   filter(
#     prob > 0
#   ) -> paths

# paths |>
#   filter(
#     occupation == 2
#   ) |>
#   filter(
#     occupation.to == 2
#   ) |>
#   arrange(-prob) ->
# dsds

# dsds |>
#   # paths |>
#   mutate(
#     inverse.payoff =
#       yap$inverse.payoff(
#         pay$payoff(
#           prob,
#           cost
#         )
#       )
#   ) |>
#   select(-id, -starts_with("vertex")) |>
#   arrange(inverse.payoff) |>
#   print(n = Inf)

# inverse expected payoff
# paths |>
#   mutate(
#     inverse.payoff = yap$inverse.payoff(
#       pay$payoff(
#         prob,
#         cost
#       )
#     )
#   ) -> paths

# endregion
# region: movement graph
# graph
# paths |>
#   select(
#     vertex,
#     vertex.to
#   ) |>
#   as.matrix() |>
#   gr$graph.edgelist(
#     directed = T
#   ) |>
#   gr$set.edge.attribute(
#     "type",
#     value = paths$type
#   ) |>
#   gr$set.edge.attribute(
#     "weight",
#     value = paths$inverse.payoff
#   ) |>
#   gr$set.edge.attribute(
#     "cost",
#     value = paths$cost
#   ) |>
#   gr$set.edge.attribute(
#     "occupation.from",
#     value = paths$occupation
#   ) |>
#   gr$set.edge.attribute(
#     "occupation.to",
#     value = paths$occupation.to
#   ) |>
#   gr$set.edge.attribute(
#     "vertex.from",
#     value = paths$vertex
#   ) |>
#   gr$set.edge.attribute(
#     "vertex.to",
#     value = paths$vertex.to
#   ) |>
#   gr$set.edge.attribute(
#     "x.from",
#     value = paths$x
#   ) |>
#   gr$set.edge.attribute(
#     "x.to",
#     value = paths$x.to
#   ) |>
#   gr$set.edge.attribute(
#     "t.from",
#     value = paths$t
#   ) |>
#   gr$set.edge.attribute(
#     "t.to",
#     value = paths$t.to
#   ) |>
#   gr$set.edge.attribute(
#     "table.id",
#     value = paths$id
#   ) |>
#   gr$set.edge.attribute(
#     "util",
#     value = 1
#   ) -> paths.graph

# endregion
# exports
# region: rds
# path list
list(
  'mod' = Sys.getenv("ATLAS_MOD") |>
    file.path(
      "roadmap",
      "path",
      "data",
      "rds",
      "paths.rds"
    ),
  'rds' = Sys.getenv("ATLAS_RDS") |>
    file.path("paths.rds")
) |>
  lapply(
    function(path) {
      list(
        expected = list(
          graph = paths.graph.expected,
          careers = careerGrid.expected,
          vertices = vertices.expected
        ),
        detailed = list(
          graph = paths.graph.expected,
          careers = careerGrid.expected,
          vertices = vertices.expected
        )
        # ,detailed = list(
        #   graph = paths.graph.detailed,
        #   careers = data$detailed$careers,
        #   vertices = data$detailed$vertices
        # )
      ) |>
        saveRDS(path)
    }
  )

# endregion
# region: csv
careerGrid.expected |>
  write_csv(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path("csv", "careers_expected.csv")
  )

vertices.expected |>
  write_csv(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path("csv", "vertices_expected.csv")
  )

# endregion
# region: parquet
careerGrid.expected |>
  write_parquet(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path("parquet", "careers_expected.parquet")
  )

vertices.expected |>
  write_parquet(
    Sys.getenv("ATLAS_OUTPUT") |>
      file.path("parquet", "vertices_expected.parquet")
  )

# endregion
