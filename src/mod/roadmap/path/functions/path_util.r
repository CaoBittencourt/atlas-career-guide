# setup
# region: imports
box::use(
  assert = utils / assert,
  roadmap / path / data / graph[...],
  gr = igraph,
  dplyr[...]
)

# endregion
# dispatch
# region: set path utility
# match each utility to an occupation
# assume vector is in the same order as occupation's indexes
set.path.util <- function(graph = paths$graph, util) {
  # assert args in main function
  return(
    graph |>
      gr$set.edge.attribute(
        "util",
        value = util |>
          as_tibble() |>
          rename(
            util = 1
          ) |>
          mutate(
            occupation = row_number()
          ) |>
          right_join(
            tibble(
              occupation.to = paths$graph |> gr$get.edge.attribute("occupation.to")
            ),
            by = c("occupation" = "occupation.to")
          ) |>
          pull(util)
      ) |>
      gr$set.edge.attribute(
        "weight",
        value = cost_util(
          gr$E(graph)$cost,
          util
        )
      )
  )
}

# endregion
# region: get path utility
path.util <- function(epath, graph = paths$graph) {
  # assert args in main function
  return(
    graph |>
      gr$get.edge.attribute(
        "util",
        epath
      )
  )
}

# endregion
# exports
# region: exports
box::export(set.path.util, path.util)

# endregion
