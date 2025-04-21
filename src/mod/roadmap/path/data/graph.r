# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  req = mod / roadmap / path / data / req,
  mod / roadmap / path / data / similarity[...],
  mod / roadmap / path / functions / grid[...],
  mod / roadmap / path / functions / cost[...],
  stats[na.omit],
  gr = igraph,
  readr[...],
  dplyr[...],
  tidyr[...],
)

# endregion
# implementation
# region: basic education
career.grid(0, 0, req$education$high.school, xmax = 0) -> basic.education

# endregion
# region: career progressions
career.grid |>
  Map(
    req$career.req$xmin,
    req$career.req$tmin
  ) ->
career.grids

# endregion
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
# region: movement types
# 1. "teleport" vertically to another occupation at a parallel vertex (same x,t)
vertices |>
  inner_join(
    vertices,
    suffix = c("", ".to"),
    by = c("x" = "x", "t" = "t"),
    relationship = "many-to-many"
  ) ->
paths.switch

# 2. or increment either experience or education within the same occupation
# with only direct (adjacent) movement, i.e.

# valid education movements
# high.school -> associate
# associate -> bachelor
# bachelor -> master
# master -> doctorate
req$education |>
  unlist() |>
  as_tibble() |>
  rename(t = 1) |>
  # as_tibble(
  #   rownames = "education"
  # ) |>
  # rename(t = 2) |>
  mutate(
    t.to = dplyr::lead(t, 1)
  ) ->
education.move

# valid experience movements
# intern -> junior
# junior -> associate
# associate -> mid.level
# mid.level -> senior
req$experience |>
  unlist() |>
  as_tibble() |>
  rename(x = 1) |>
  # as_tibble(
  #   rownames = "experience"
  # ) |>
  # rename(x = 2) |>
  mutate(
    x.to = dplyr::lead(x, 1)
  ) ->
experience.move

# increase education
vertices |>
  inner_join(
    education.move,
    by = c("t" = "t"),
  ) |>
  na.omit() ->
paths.study

# increase experience
vertices |>
  inner_join(
    experience.move,
    by = c("x" = "x")
  ) |>
  na.omit() ->
paths.work

# all valid paths
bind_rows(
  paths.switch |>
    mutate(
      x.to = x,
      t.to = t
    ),
  paths.work |>
    inner_join(
      vertices,
      suffix = c("", ".to"),
      by = c(
        "x.to" = "x",
        "t" = "t",
        "occupation" = "occupation"
      )
    ) |>
    mutate(
      occupation.to = occupation,
      t.to = t
    ),
  paths.study |>
    inner_join(
      vertices,
      suffix = c("", ".to"),
      by = c(
        "x" = "x",
        "t.to" = "t",
        "occupation" = "occupation"
      )
    ) |>
    mutate(
      occupation.to = occupation,
      x.to = x
    )
) |>
  filter(
    vertex != vertex.to
  ) |>
  relocate(
    starts_with("vertex"),
    starts_with("occupation"),
    starts_with("x"),
    starts_with("t")
  ) ->
paths

# endregion
# region: movement cost
mtx_similarity |>
  mutate(
    id = row_number(),
    occupation = to
  ) |>
  select(
    id,
    occupation
  ) ->
ids

mtx_similarity |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "similarity"
  ) |>
  inner_join(
    ids |> rename(id.to = id),
    by = c(
      "to" = "occupation"
    )
  ) |>
  inner_join(
    ids |> rename(id.from = id),
    by = c(
      "from" = "occupation"
    )
  ) |>
  select(
    occupation = id.from,
    occupation.to = id.to,
    similarity
  ) |>
  inner_join(
    paths
  ) |>
  mutate(
    cost = move.cost(
      skq = similarity,
      xk = x,
      xq = x.to,
      tk = t,
      tq = t.to
    )
  ) ->
paths

# endregion
# region: movement graph
# feasible paths
paths |>
  filter(
    !is.infinite(cost)
  ) ->
paths

# graph
paths |>
  select(
    vertex,
    vertex.to
  ) |>
  as.matrix() |>
  gr$graph.edgelist(
    directed = T
  ) |>
  gr$set.edge.attribute(
    "weight",
    value = paths$cost
  ) ->
paths.graph

# endregion
# exports
# region: exports
# paths list
list(
  graph = paths.graph,
  table = paths
) -> paths

box::export(paths)

# endregion
