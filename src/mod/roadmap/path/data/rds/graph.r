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
  roadmap / path / data / similarity[...],
  req = roadmap / path / data / req,
  lab = roadmap / path / data / labor,
  dt = dtplyr,
  roadmap / path / data / vertices[...],
  pay = roadmap / path / functions / payoff,
  yap = roadmap / path / functions / payoff_inverse,
  utils / data[sublist],
  stats[na.omit],
  gr = igraph,
  readr[...],
  dplyr[...],
  tidyr[...],
)

# endregion
# model
# region: simplified model
careers |> mutate(.before = 1, id = row_number()) -> careers

careers |>
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
    value = careers$cost.expected
  ) |>
  gr$set.edge.attribute(
    "cost",
    value = careers$cost.expected
  ) |>
  gr$set.edge.attribute(
    "occupation.from",
    value = careers$career
  ) |>
  gr$set.edge.attribute(
    "occupation.to",
    value = careers$careerTo
  ) |>
  gr$set.edge.attribute(
    "vertex.from",
    value = careers$career
  ) |>
  gr$set.edge.attribute(
    "vertex.to",
    value = careers$careerTo
  ) |>
  gr$set.edge.attribute(
    "x.from",
    value = careers$x.expected
  ) |>
  gr$set.edge.attribute(
    "x.to",
    value = careers$x.expected.to
  ) |>
  gr$set.edge.attribute(
    "t.from",
    value = careers$t.expected
  ) |>
  gr$set.edge.attribute(
    "t.to",
    value = careers$t.expected.to
  ) |>
  gr$set.edge.attribute(
    "table.id",
    value = careers$id
  ) |>
  gr$set.edge.attribute(
    "util",
    value = 1
  ) -> paths.graph


df_ids |>
  inner_join(
    path(1L, 2L, graph = paths.graph) |>
      path.timeline(graph = paths.graph) |>
      rename(id = occupation)
  ) |>
  select(
    year,
    duration,
    occupation,
    id
  )


# endregion
# # region: movement types
# # 1. "teleport" vertically to another occupation at a parallel vertex (same x,t)
# expand.grid(
#   vertex = vertices$vertex,
#   vertex.to = vertices$vertex
# ) -> vertices.comb

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
#   ) -> paths.switch

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

# # 2. increment either experience or education within the same occupation
# # with only direct (adjacent) movement, i.e.
# # or decrement either experience or education within the same occupation
# # with only direct (adjacent) movement and zero cost, i.e.

# # valid education movements
# # high.school -> associate
# # associate -> bachelor
# # bachelor -> master
# # master -> doctorate

# # high.school <- associate
# # associate <- bachelor
# # bachelor <- master
# # master <- doctorate
# req$education |>
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

# # valid experience movements
# # intern -> junior
# # junior -> associate
# # associate -> mid.level
# # mid.level -> senior

# # intern <- junior
# # junior <- associate
# # associate <- mid.level
# # mid.level <- senior
# req$experience |>
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

# # education move
# vertices |>
#   inner_join(
#     education.move,
#     by = c("t" = "t"),
#     relationship = "many-to-many"
#   ) |>
#   na.omit() -> paths.study

# # experience move
# vertices |>
#   inner_join(
#     experience.move,
#     by = c("x" = "x"),
#     relationship = "many-to-many"
#   ) |>
#   na.omit() -> paths.work

# # 3. teleport back to basic education (hard reset)
# expand.grid(
#   vertex = vertices |>
#     filter(
#       occupation !=
#         (req$df_ids |> filter(occupation == "Basic Education") |> pull(id))
#     ) |>
#     pull(vertex),
#   vertex.to = vertices |>
#     filter(
#       occupation ==
#         (req$df_ids |> filter(occupation == "Basic Education") |> pull(id))
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

# # # 3. teleport to first vertex of any occupation at full (x.to + t.to) cost? (hard reset)
# # expand.grid(
# #   vertex = vertices$vertex,
# #   vertex.to =
# #     vertices |>
# #       group_by(
# #         occupation
# #       ) |>
# #       slice(1) |>
# #       ungroup() |>
# #       pull(vertex)
# # ) |>
# #   inner_join(
# #     vertices,
# #     by = c("vertex" = "vertex"),
# #     relationship = "many-to-many"
# #   ) |>
# #   inner_join(
# #     vertices,
# #     suffix = c("", ".to"),
# #     by = c("vertex.to" = "vertex"),
# #     relationship = "many-to-many"
# #   ) |>
# #   filter(
# #     occupation != occupation.to
# #   ) |>
# #   mutate(
# #     type = "reset"
# #   ) ->
# # paths.restart

# # all valid paths
# bind_rows(
#   # switch careers
#   paths.switch |>
#     mutate(
#       x.to = x,
#       t.to = t
#     ),
#   # reset career
#   paths.reset,
#   # # restart career
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
mtx_similarity |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "similarity"
  ) |>
  inner_join(
    req$df_ids |> rename(id.to = id),
    by = c(
      "to" = "occupation"
    )
  ) |>
  inner_join(
    req$df_ids |> rename(id.from = id),
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
  ) -> paths

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
  ) -> paths

# endregion
# region: movement payoff
# feasible paths
paths |>
  filter(
    !is.infinite(cost)
  ) |>
  mutate(
    .before = 1,
    id = row_number()
  ) |>
  select(-prob) |>
  rename(
    prob = prob.to
  ) |>
  filter(
    prob > 0
  ) -> paths

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
paths |>
  mutate(
    inverse.payoff = yap$inverse.payoff(
      pay$payoff(
        prob,
        cost
      )
    )
  ) -> paths

# endregion
# region: movement graph
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
    value = paths$inverse.payoff
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
  ) -> paths.graph

# endregion
# exports
# region: exports
# path list
list(
  graph = paths.graph,
  table = paths,
  vertices = vertices
) |>
  saveRDS(
    Sys.getenv("ATLAS_MOD") |>
      file.path(
        "roadmap",
        "path",
        "data",
        "rds",
        "paths.rds"
      )
  )

# endregion
