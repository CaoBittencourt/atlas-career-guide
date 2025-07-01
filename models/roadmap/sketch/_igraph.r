options(box.path = Sys.getenv("ATLAS_MOD"))

box::use(
  gr = igraph,
  utils / data[sublist],
  readr[...],
  dplyr[...],
  tidyr[...],
)

# required education
list(
  high.school = 17,
  associate = 19,
  bachelor = 21,
  master = 23,
  doctorate = 28
) -> education

education |>
  lapply(
    function(t) {
      t - education$high.school
    }
  ) ->
restart

# required experience
list(
  intern = 0,
  junior = 2,
  associate = 3,
  mid.level = 5,
  senior = 10
) -> experience

# career progression map for one occupation (experience vs education 2d grid)
career.grid <- function(xmin, tmin, xmax = NULL, tmax = education$doctorate) {
  return(
    expand.grid(
      # x = xmin:ifelse(!length(xmax), tbar, xmax), # experience[experience >= xmin],
      x = experience |> sublist(function(x) ifelse(!length(xmax), x >= xmin, x >= xmin & x <= xmax)) |> as.numeric(),
      t = education |> sublist(function(t) (t >= tmin) & (t <= tmax)) |> as.numeric()
    )
  )
}

# basic education
career.grid(0, 0, education$high.school, xmax = 0) -> basic.education

# minimum required education
"atlas.education" |>
  getOption() |>
  readRDS() |>
  mutate(
    id = row_number()
  ) |>
  select(
    id,
    occupation,
    tmin = education_years
  ) ->
min.edu

# use job zone to estimate minimum required experience
"atlas.oldata" |>
  getOption() |>
  read.csv() |>
  mutate(
    xmin = ifelse(id_zone < 2, 0, id_zone)
  ) |>
  select(
    occupation,
    xmin
  ) ->
min.xp

# career requirements data frame
min.edu |>
  inner_join(
    min.xp,
    by = c(
      "occupation" = "occupation"
    )
  ) ->
career.req

# career grids
career.grid |>
  Map(
    career.req$xmin,
    career.req$tmin
  ) ->
career.grids

career.grids[1]

# c(
#   career.grids,
#   list(basic.education)
# ) -> career.grids

# career.grids[[1]]
# career.grids[[length(career.grids)]]

# similarity matrix
career.req |>
  select(
    to = occupation
  ) |>
  inner_join(
    "atlas.root" |>
      getOption() |>
      file.path(
        "articles",
        "1.introduction-matching",
        "output",
        "similarity_matrix.csv"
      ) |>
      read_csv() |>
      rename(
        to = 1
      ) |>
      relocate(
        career.req$
          occupation
      )
  ) |>
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~.x
      # .fns = ~ .x^2
    )
  ) ->
mtx_similarity

# mtx_similarity |> mutate(`Basic Education` = 1) -> mtx_similarity

# mtx_similarity |>
#   bind_rows(
#     mtx_similarity |>
#       slice(1) |>
#       mutate(
#         to = "Basic Education",
#         across(
#           .cols = where(is.numeric),
#           .fns = ~1
#         )
#       )
#   ) ->
# mtx_similarity

# career move (note: movement within the same career uses this function as well)
career.move <- function(skq, xk, xq, tk, tq) {
  # # remove baseline education
  # tk <- tk - education$high.school
  # tq <- tq - education$high.school

  # equivalent similarity
  skq.eq <- ((skq^2) >= 0.5) * skq
  # skq.eq <- (skq >= 0.5) * skq

  # xp and edu requirements
  (xq - xk * skq.eq) -> req.x
  (tq - tk * skq.eq) -> req.t

  (req.x > 0) * req.x -> req.x
  (req.t > 0) * req.t -> req.t

  # allow for educational restart (via education "occupation")
  # for the first levels of education
  # e.g. start new major from scratch
  # carry-over education and experience: req.t and req.x
  # carry-over experience, restart education: tq - restart and req.x
  # full restart: tq - restart and xq
  ifelse(
    xq == education$associate & req.t > restart$associate,
    restart$associate,
    req.t
  ) ->
  req.t

  ifelse(
    xq == education$bachelor & req.t > restart$bachelor,
    restart$bachelor,
    req.t
  ) ->
  req.t

  # career move duration in years
  return((req.x + req.t) / skq)
}

# career.move <- function(skq, xk, xq, tk, tq) {
#   # # remove baseline education
#   # tk <- tk - education$high.school
#   # tq <- tq - education$high.school

#   # equivalent similarity
#   skq.eq <- ((skq^2) >= 0.5) * skq
#   # skq.eq <- (skq >= 0.5) * skq

#   # xp and edu requirements
#   (xq - xk * skq.eq) -> req.x
#   (tq - tk * skq.eq) -> req.t

#   (req.x > 0) * req.x -> req.x
#   (req.t > 0) * req.t -> req.t

#   req.x / skq -> req.x
#   req.t / skq -> req.t

#   # allow for educational restart
#   # for the first levels of education
#   # e.g. start new major from scratch
#   ifelse(
#     xq == education$associate & req.t > restart$associate,
#     restart$associate,
#     req.t
#   ) ->
#   req.t

#   ifelse(
#     xq == education$bachelor & req.t > restart$bachelor,
#     restart$bachelor,
#     req.t
#   ) ->
#   req.t

#   # total career move duration
#   return(req.x + req.t)
# }

# vertices
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

# valid movement types
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
education |>
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
experience |>
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

# # vertices |>
# #   # rename_with(
# #   #   ~ paste0(.x, ".from")
# #   # ) |>
# #   inner_join(
# #     vertices
# #     #   |>
# #     #   rename_with(
# #     #   ~ paste0(.x, ".to")
# #     # ),
# #     ,
# #     suffix = c('','.to')
# #     ,by = c(
# #       "x.from" = "x.to",
# #       "t.from" = "t.to"
# #     )
# #     # ,by = c(
# #     #   "x.from" = "x.to",
# #     #   "t.from" = "t.to"
# #     # ),
# #     relationship = "many-to-many"
# #   ) |>
# #   filter(
# #     vertex.from != vertex.to
# #   ) |>
# #   rename(
# #     vertex.from,
# #     vertex.to,
# #     occupation.from,
# #     occupation.to,
# #     x = x.from,
# #     t = t.from
# #   )

# # expand.grid(
# #   from = vertices$vertex,
# #   to = vertices$vertex
# # ) ->
# # vertices.combn

# vertices -> vertices.from
# vertices -> vertices.to

# names(vertices.from) |> paste0(".from") -> names(vertices.from)
# names(vertices.to) |> paste0(".to") -> names(vertices.to)

# vertices.combn |>
#   inner_join(
#     vertices.from,
#     by = c("from" = "vertex.from")
#   ) |>
#   inner_join(
#     vertices.to,
#     by = c("to" = "vertex.to")
#   ) ->
# vertices.combn

# # vertices.combn |>
# #   filter(!(
# #     occupation.from == occupation.to &
# #       x.from >= x.to & t.from >= t.to
# #   )) ->
# # vertices.combn

# movement cost
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
    cost = career.move(
      skq = similarity,
      xk = x,
      xq = x.to,
      tk = t,
      tq = t.to
    )
  ) ->
paths

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
career.graph

paths |>
  filter(occupation == 3) |>
  filter(occupation.to == 1)

vertices |> filter(occupation == 2)
vertices |> filter(occupation == 1)
vertices |> filter(vertex == 8)
ids |> slice(vertices |> filter(vertex == 3922) |> pull(occupation))
paths |>
  filter(
    occupation ==
      vertices |>
        filter(vertex == 3926) |>
        pull(occupation)
  )

# shortest path
ids |> slice(vertices |> filter(vertex == 7) |> pull(occupation))
ids |> slice(vertices |> filter(vertex == 4) |> pull(occupation))

ids |> slice(vertices |> filter(vertex == 7) |> pull(occupation))
ids |> slice(vertices |> filter(vertex == 8) |> pull(occupation))
ids |> slice(vertices |> filter(vertex == 3922) |> pull(occupation))
paths |>
  filter(vertex == 8) |>
  filter(vertex.to == 3922)
paths |>
  filter(occupation == 2) |>
  filter(occupation.to != occupation) |>
  arrange(cost)

career.graph |>
  gr$shortest_paths(
    from = 7,
    to = 4,
    output = "epath",
    algorithm = "dijkstra"
  )

career.graph |>
  gr$shortest_paths(
    from = 23,
    to = 6,
    output = "vpath",
    algorithm = "dijkstra"
  )

paths |>
  filter(
    vertex %in% (
      (
        career.graph |>
          gr$shortest_paths(
            from = 7,
            to = 4,
            output = "vpath",
            algorithm = "dijkstra"
          )
      )$vpath |>
        unlist()
    )
  )


career.graph |>
  gr$edge.attributes()

career.graph |>
  gr$get.edge.attribute(
    "weight",
    unlist(
      (
        career.graph |>
          gr$shortest_paths(
            from = 7,
            to = 4,
            output = "epath",
            algorithm = "dijkstra"
          )
      )$epath
    )
  )

paths

vertex.from <- vertices |>
  filter(occupation == 5) |>
  slice(1) |>
  pull(vertex)
vertex.to <- vertices |>
  filter(occupation == 28) |>
  slice(1) |>
  pull(vertex)

ids |> slice(5, 28)

career.graph |>
  gr$shortest_paths(
    from = vertex.from,
    to = vertex.to,
    output = "vpath"
  )

career.graph |>
  gr$get.edge.attribute(
    "weight",
    unlist(
      (
        career.graph |>
          gr$shortest_paths(
            from = vertex.from,
            to = vertex.to,
            output = "epath"
          )
      )$epath
    )
  ) ->
move.cost

move.cost |> sum()

# mtx_similarity |>
#   pivot_longer(
#     cols = -1,
#     names_to = "from",
#     values_to = "similarity"
#   ) |>
#   inner_join(
#     ids |> rename(id.to = id),
#     by = c(
#       "to" = "occupation"
#     )
#   ) |>
#   inner_join(
#     ids |> rename(id.from = id),
#     by = c(
#       "from" = "occupation"
#     )
#   ) |>
#   select(
#     occupation.to = id.to,
#     occupation.from = id.from,
#     similarity
#   ) |>
#   right_join(
#     vertices.combn,
#     relationship = "many-to-many",
#     multiple = "all",
#   ) ->
# vertices.combn

# vertices.combn |>
#   mutate(
#     cost = career.move(
#       similarity,
#       x.from,
#       x.to,
#       t.from,
#       t.to
#     )
#   ) ->
# vertices.combn

# vertices.combn |>
#   mutate(
#     cost = ifelse(
#       is.na(cost) & similarity < 0.5,
#       Inf,
#       cost
#     )
#   ) ->
# vertices.combn

# vertices.combn |>
#   filter(
#     !is.infinite(cost)
#   ) ->
# vertices.combn

# vertices.combn |>
#   select(
#     from,
#     to,
#     cost
#   ) |>
#   pivot_wider(
#     id_cols = "from",
#     names_from = "to",
#     values_from = "cost"
#   ) |>
#   select(-1) |>
#   as.matrix() |>
#   gr$graph.adjacency(
#     mode = "directed",
#     weighted = T
#   ) ->
# career.graph

# # adjency matrix
# cbind(
#   a = c(0, 19, 1, 9),
#   b = c(19, 0, 9, 1),
#   c = c(9, 19, 0, 1),
#   d = c(9, 1, 19, 0)
# ) -> adj

# cbind(
#   a = c(0, 19, 1, 9),
#   b = c(19, 0, 9, 1),
#   c = c(9, 19, 0, 1),
#   d = c(9, 1, 19, 0)
# ) *
#   cbind(
#     a = 1 - rep(1, 4),
#     b = 1 - rep(0.19, 4),
#     c = 1 - rep(.5, 4),
#     d = 1 - rep(.19, 4)
#   ) -> wgt

# colnames(adj) -> rownames(adj)
# colnames(wgt) -> rownames(wgt)

# adj
# wgt

# # graph struct
# adj |>
#   gr$graph.adjacency(
#     mode = "directed",
#     weighted = T
#   ) ->
# x

# wgt |>
#   gr$graph.adjacency(
#     mode = "directed",
#     weighted = T
#   ) ->
# x

# shortest path
# career.graph |>
#   gr$shortest_paths(
#     from = vertices.combn |>
#       select(
#         occupation.from,
#         from
#       ) |>
#       filter(
#         occupation.from == 874
#       ) |>
#       unique() |>
#       pull(from) |>
#       min(),
#     to = vertices.combn |>
#       select(
#         occupation.from,
#         from
#       ) |>
#       filter(
#         occupation.from == 1
#       ) |>
#       unique() |>
#       pull(from) |>
#       min(),
#     algorithm = "dijkstra",
#     output = "epath"
#   )


# x |>
#   gr$shortest_paths(
#     from = (x |> V() |> names() == "a") |> which(),
#     to = (x |> V() |> names() == "b") |> which(),
#     algorithm = "dijkstra",
#     output = "epath"
#   ) ->
# xpath

# # # shortest path
# # x |>
# #   distances(
# #     v = (x |> V() |> names() == "a") |> which(),
# #     to = V(x),
# #     algorithm = "dijkstra"
# #   )

# # x |>
# #   distances(
# #     v = (x |> V() |> names() == "a") |> which(),
# #     to = (x |> V() |> names() == "b") |> which(),
# #     algorithm = "dijkstra"
# #   )
