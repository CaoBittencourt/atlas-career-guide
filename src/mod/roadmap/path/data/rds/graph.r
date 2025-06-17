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
  mod / roadmap / path / data / vertices[...],
  mod / utils / data[sublist],
  stats[na.omit],
  gr = igraph,
  readr[...],
  dplyr[...],
  tidyr[...],
)

# endregion
# model
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

vertices |>
  inner_join(
    vertices,
    suffix = c("", ".to"),
    by = c("x" = "x", "t" = "t"),
    relationship = "many-to-many"
  ) |>
  mutate(
    type = "switch"
  ) ->
paths.switch

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

education.move |>
  bind_rows(
    education.move |>
      rename(
        t.to = t,
        t = t.to
      ) |>
      na.omit()
  ) |>
  mutate(
    type = "study"
  ) ->
education.move

# valid experience movements
# intern -> junior
# junior -> associate
# associate -> mid.level
# mid.level -> senior

# intern <- junior
# junior <- associate
# associate <- mid.level
# mid.level <- senior
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

experience.move |>
  bind_rows(
    experience.move |>
      rename(
        x.to = x,
        x = x.to
      ) |>
      na.omit()
  ) |>
  mutate(
    type = "work"
  ) ->
experience.move

# education move
vertices |>
  inner_join(
    education.move,
    by = c("t" = "t"),
    relationship = "many-to-many"
  ) |>
  na.omit() ->
paths.study

# experience move
vertices |>
  inner_join(
    experience.move,
    by = c("x" = "x"),
    relationship = "many-to-many"
  ) |>
  na.omit() ->
paths.work

# 3. teleport back to basic education (hard reset)
expand.grid(
  vertex = vertices$vertex,
  vertex.to =
    vertices |>
      filter(
        occupation == 874
      ) |>
      slice(1) |>
      pull(vertex)
) |>
  inner_join(
    vertices,
    by = c("vertex" = "vertex"),
    relationship = "many-to-many"
  ) |>
  inner_join(
    vertices,
    suffix = c("", ".to"),
    by = c("vertex.to" = "vertex"),
    relationship = "many-to-many"
  ) |>
  filter(
    occupation != occupation.to
  ) |>
  mutate(
    type = "reset"
  ) ->
paths.reset

# 3. teleport to first vertex of any occupation at full (x.to + t.to) cost? (hard reset)
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
bind_rows(
  # switch careers
  paths.switch |>
    mutate(
      x.to = x,
      t.to = t
    ),
  # reset career
  paths.reset,
  # move experience
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
  # move education
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
  select(
    -ends_with("pct.to")
  ) |>
  relocate(
    starts_with("vertex"),
    starts_with("occupation"),
    type,
    starts_with("x"),
    starts_with("t")
  ) ->
paths

# endregion
# region: movement similarity
mtx_similarity |>
  mutate(
    `Basic Education` = 1
  ) ->
mtx_similarity

mtx_similarity |>
  bind_rows(
    c(
      "Basic Education",
      rep(1, 874) |> as.list()
    ) |>
      setNames(
        mtx_similarity |>
          names()
      ) |>
      as_tibble()
  ) ->
mtx_similarity

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
  ) ->
paths

# endregion
# region: movement cost
paths |>
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
# region: movement payoff
# - E[U] = Pr[v2 | v1] * u(v2) = ((w(v2) / w) * s(v1, v2) * [s(v1, v2) >= 0.5]) * u(v2) >= 0
#         - cost(v1,v2)
#         - weight := cost * ((1 - E[u]) ^ !is.infinity(cost))

# endregion
# region: movement graph
# feasible paths
paths |>
  filter(
    !is.infinite(cost)
  ) |>
  mutate(
    .before = 1,
    id = row_number()
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
    "type",
    value = paths$type
  ) |>
  gr$set.edge.attribute(
    "weight",
    value = paths$cost
  ) |>
  gr$set.edge.attribute(
    "cost",
    value = paths$cost
  ) |>
  gr$set.edge.attribute(
    "occupation.from",
    value = paths$occupation
  ) |>
  gr$set.edge.attribute(
    "occupation.to",
    value = paths$occupation.to
  ) |>
  gr$set.edge.attribute(
    "vertex.from",
    value = paths$vertex
  ) |>
  gr$set.edge.attribute(
    "vertex.to",
    value = paths$vertex.to
  ) |>
  gr$set.edge.attribute(
    "x.from",
    value = paths$x
  ) |>
  gr$set.edge.attribute(
    "x.to",
    value = paths$x.to
  ) |>
  gr$set.edge.attribute(
    "t.from",
    value = paths$t
  ) |>
  gr$set.edge.attribute(
    "t.to",
    value = paths$t.to
  ) |>
  gr$set.edge.attribute(
    "table.id",
    value = paths$id
  ) |>
  gr$set.edge.attribute(
    "util",
    value = 1
  ) ->
paths.graph

# endregion
# exports
# region: exports
# vertices
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

# paths list
list(
  graph = paths.graph,
  table = paths,
  vertices = vertices
) |>
  saveRDS(
    getOption("atlas.mod") |>
      file.path(
        "roadmap",
        "path",
        "data",
        "rds",
        "paths.rds"
      )
  )

# endregion
