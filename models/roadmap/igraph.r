modular::project.options("atlas")

box::use(
  gr = igraph,
  mod / utils / data[sublist],
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
list(
  entry.level = 0,
  low.level = 1,
  mid.level = 3,
  high.level = 5,
  top.level = 10
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

basic.education

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

min.edu |>
  inner_join(
    min.xp,
    by = c(
      "occupation" = "occupation"
    )
  ) ->
career.req

career.grid |>
  Map(
    career.req$xmin,
    career.req$tmin
  ) ->
career.grids

career.grids[1]

# career move (note: movement within the same career uses this function as well)
career.move <- function(skq, xk, xq, tk, tq) {
  # career move duration in years
  (xq - xk * skq) -> req.x
  (tq - tk * skq) -> req.t

  (req.x > 0) * req.x -> req.x
  (req.t > 0) * req.t -> req.t

  return((req.x + req.t) / skq)
}

# example: movement within the same occupation
career.grids[[1]][1, ] -> from
career.grids[[1]][2, ] -> to
career.move(1, from$x[[1]], to$x[[1]], from$t, to$t)

# example: career switch


# adjency matrix
cbind(
  a = c(0, 19, 1, 9),
  b = c(19, 0, 9, 1),
  c = c(9, 19, 0, 1),
  d = c(9, 1, 19, 0)
) -> adj

cbind(
  a = c(0, 19, 1, 9),
  b = c(19, 0, 9, 1),
  c = c(9, 19, 0, 1),
  d = c(9, 1, 19, 0)
) *
  cbind(
    a = 1 - rep(1, 4),
    b = 1 - rep(0.19, 4),
    c = 1 - rep(.5, 4),
    d = 1 - rep(.19, 4)
  ) -> wgt

colnames(adj) -> rownames(adj)
colnames(wgt) -> rownames(wgt)

adj
wgt

# graph struct
adj |>
  gr$graph.adjacency(
    mode = "directed",
    weighted = T
  ) ->
x

wgt |>
  gr$graph.adjacency(
    mode = "directed",
    weighted = T
  ) ->
x

# shortest path
x |>
  gr$shortest_paths(
    from = (x |> V() |> names() == "a") |> which(),
    to = (x |> V() |> names() == "b") |> which(),
    algorithm = "dijkstra",
    output = "epath"
  ) ->
xpath

# # shortest path
# x |>
#   distances(
#     v = (x |> V() |> names() == "a") |> which(),
#     to = V(x),
#     algorithm = "dijkstra"
#   )

# x |>
#   distances(
#     v = (x |> V() |> names() == "a") |> which(),
#     to = (x |> V() |> names() == "b") |> which(),
#     algorithm = "dijkstra"
#   )
