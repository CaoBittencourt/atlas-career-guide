modular::project.options("atlas")

box::use(
  gr = igraph,
  mod / utils / data[sublist],
  readr[...],
  dplyr[...],
  tidyr[...],
)

# required education
list(
  elementary = 14,
  high.school = 17,
  associate = 19,
  bachelor = 21,
  master = 23,
  doctorate = 28
) -> education

# required experience
# list(
#   entry.level = 0,
#   low.level = 1,
#   mid.level = 3,
#   high.level = 5,
#   top.level = 10
# ) -> experience
list(
  entry.level = 0,
  mid.level = 3,
  high.level = 7
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

# basic.education

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

c(
  career.grids,
  list(basic.education)
) -> career.grids

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
      .fns = ~ .x^2
    )
  ) ->
mtx_similarity

mtx_similarity |> mutate(`Basic Education` = 1) -> mtx_similarity

mtx_similarity |>
  bind_rows(
    mtx_similarity |>
      slice(1) |>
      mutate(
        to = "Basic Education",
        across(
          .cols = where(is.numeric),
          .fns = ~1
        )
      )
  ) ->
mtx_similarity

# career move (note: movement within the same career uses this function as well)
career.move <- function(skq, xk, xq, tk, tq) {
  # remove baseline education
  ifelse(
    tq >= education$high.school,
    education$high.school,
    education$elementary
  ) -> tbase

  tk <- tk - tbase
  tq <- tq - tbase

  # equivalent similarity
  skq.eq <- (skq >= 0.5) * skq

  # xp and edu requirements
  (xq - xk * skq.eq) -> req.x
  (tq - tk * skq.eq) -> req.t

  (req.x > 0) * req.x -> req.x
  (req.t > 0) * req.t -> req.t

  # career move duration in years
  return((req.x + req.t) / skq.eq)
}

# # example: movement within the same occupation
# career.grids[[1]][1, ] -> from
# career.grids[[1]][2, ] -> to
# career.move(1, from$x, to$x, from$t, to$t)

# # example: movement within the same occupation
# career.grids[[1]][1, ] -> from
# career.grids[[1]][3, ] -> to
# career.move(1, from$x, to$x, from$t, to$t)

# # example: career switch
# career.grids[[length(career.grids)]][2, ] -> from
# career.grids[[1]][1, ] -> to
# career.move(
#   1,
#   from$x,
#   to$x,
#   from$t,
#   to$t
# )

# # example: career switch
# career.grids[[1]][1, ] -> from
# career.grids[[3]][1, ] -> to
# mtx_similarity |>
#   select(-1) |>
#   slice(3) |>
#   pull(1) |>
#   career.move(
#     from$x,
#     to$x,
#     from$t,
#     to$t
#   )

# # example: career switch
# career.grids[[1]][1, ] -> from
# career.grids[[2]][1, ] -> to
# mtx_similarity |>
#   select(-1) |>
#   slice(2) |>
#   pull(1) |>
#   career.move(
#     from$x,
#     to$x,
#     from$t,
#     to$t
#   )

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

expand.grid(
  from = vertices$vertex,
  to = vertices$vertex
) ->
vertices.combn

vertices -> vertices.from
vertices -> vertices.to

names(vertices.from) |> paste0(".from") -> names(vertices.from)
names(vertices.to) |> paste0(".to") -> names(vertices.to)

vertices.combn |>
  inner_join(
    vertices.from,
    by = c("from" = "vertex.from")
  ) |>
  inner_join(
    vertices.to,
    by = c("to" = "vertex.to")
  ) ->
vertices.combn

# vertices.combn |>
#   filter(!(
#     occupation.from == occupation.to &
#       x.from >= x.to & t.from >= t.to
#   )) ->
# vertices.combn

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
    occupation.to = id.to,
    occupation.from = id.from,
    similarity
  ) |>
  right_join(
    vertices.combn,
    relationship = "many-to-many",
    multiple = "all",
  ) ->
vertices.combn

vertices.combn |>
  mutate(
    cost = career.move(
      similarity,
      x.from,
      x.to,
      t.from,
      t.to
    )
  ) ->
vertices.combn

vertices.combn |>
  mutate(
    cost = ifelse(
      is.na(cost) & similarity < 0.5,
      Inf,
      cost
    )
  ) ->
vertices.combn

# vertices.combn |>
#   filter(
#     !is.infinite(cost)
#   ) ->
# vertices.combn

vertices.combn |>
  select(
    from,
    to,
    cost
  ) |>
  pivot_wider(
    id_cols = "from",
    names_from = "to",
    values_from = "cost"
  ) |>
  select(-1) |>
  as.matrix() |>
  gr$graph.adjacency(
    mode = "directed",
    weighted = T
  ) ->
career.graph

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
career.graph |>
  gr$shortest_paths(
    from = vertices.combn |>
      select(
        occupation.from,
        from
      ) |>
      filter(
        occupation.from == 874
      ) |>
      unique() |>
      pull(from) |>
      min(),
    to = vertices.combn |>
      select(
        occupation.from,
        from
      ) |>
      filter(
        occupation.from == 1
      ) |>
      unique() |>
      pull(from) |>
      min(),
    algorithm = "dijkstra",
    output = "epath"
  )

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
