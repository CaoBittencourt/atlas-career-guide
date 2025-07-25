# setup
# region: imports
box::use(
  assert = utils / assert,
  gr = igraph,
  roadmap / path / data[...],
)

# endregion
# dispatch
# region: which.path generic function
which.path <- function(epath, graph = paths$expected$graph) {
  # assert args in main function

  # get occupation id for each career progression
  return(
    graph |>
      gr$get.edge.attribute(
        "occupation.from",
        epath[1]
      ) |>
      c(
        graph |>
          gr$get.edge.attribute(
            "occupation.to",
            epath
          )
      )
  )
}

# endregion
# exports
# region: exports
box::export(which.path)

# endregion
