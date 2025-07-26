# setup
# region: imports
box::use(
  gr = igraph,
  roadmap / path / data[...],
)

# endregion
# dispatch
# region: get path cost (in years)
path.cost <- function(epath, graph = paths$expected$graph) {
  # assert args in main function
  return(
    graph |>
      gr$get.edge.attribute(
        "cost",
        epath
      )
  )
}

# endregion
# region: path cost with expected graph
path.cost.expected <- function(epath) {
  # assert args in main function
  return(
    path.cost(
      epath,
      paths$expected$graph
    )
  )
}

# endregion
# region: path cost with detailed graph
path.cost.detailed <- function(epath) {
  # assert args in main function
  return(
    path.cost(
      epath,
      paths$detailed$graph
    )
  )
}

# endregion
# exports
# region: exports
box::export(
  path.cost,
  path.cost.expected,
  path.cost.detailed
)

# endregion
