# setup
# region: imports
box::use(
  assert = utils / assert,
  gr = igraph,
  roadmap / path / data / req[...],
  roadmap / path / functions / path_util[...],
  roadmap / path / data / graph[...],
  dplyr[...],
)

# endregion
# methods
# region: dijkstra method
path.dijkstra <- function(graph, from, to) {
  # assert args in main function
  # dijkstra algorithm
  return(
    unlist(
      (graph |>
        gr$shortest_paths(
          from = from,
          to = to,
          output = "epath",
          algorithm = "dijkstra"
        ))$epath
    )
  )
}

# endregion
# region: list of methods
list(
  "dijkstra" = "dijkstra"
) -> path.methods

# endregion
# dispatch
# region: path generic function
path <- function(
  to,
  from = NULL,
  util = NULL,
  use.vertices = F,
  graph = paths$graph,
  path_method = path.methods[[1]],
  ...
) {
  # assert args
  # paths$table$vertex |> min() -> vertex.min
  # paths$table$vertex |> max() -> vertex.max
  # assert$base$validate.numeric.bounded(from, "from", F, vertex.min, vertex.max)
  # assert$base$validate.numeric.bounded(to, "to", F, vertex.min, vertex.max)
  assert$base$validate.bool(use.vertices, "use.vertices")
  stopifnot(any(is.integer(from), is.null(from)))
  stopifnot(is.integer(to))
  assert$base$validate.numeric.bounded(util, "util", T, 0)
  # stopifnot(
  #   length(util) == paths$table$occupation |> unique() |> length()
  # )
  stopifnot(gr$is.igraph(graph))
  assert$base$validate.method(path_method, "path_method", path.methods, F)

  # career-based model
  # vertex-based model
  if (use.vertices) {}

  # utility-adjusted weights
  if (length(util)) {
    graph |> set.path.util(util) -> graph
  }

  # multiple dispatch
  if (path_method[[1]] == path.methods$dijkstra) {
    return(path.dijkstra(graph, from, to))
  }
}

# endregion
# exports
# region: exports
box::export(path)

# endregion
