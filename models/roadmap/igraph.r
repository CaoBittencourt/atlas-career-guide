box::use(
  gr = igraph
)

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
    a =  1 - rep(1, 4),
    b =  1 - rep(0.19, 4),
    c =  1 - rep(.5, 4),
    d =  1 - rep(.19, 4)
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

xpath$epath |> gr$

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
