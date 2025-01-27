modular::project.options("atlas")
# region: imports
box::use(
  assert = mod / utils / assert[...]
)

# endregion
# region: concave utility aggregation

# endregion
# region: linear utility aggregation
agg.linear <- function() {

}

# endregion
# region: convex utility aggregation

# endregion
# region: job satisfaction dispatch function
agg.utility <- function(pref_set, skill_mtx, agg.method = c("linear", "concave", "convex")[[1]], util.fn) {
  # assert args
  assert$as.skill_mtx(pref_set) -> Uk
  assert$as.skill_mtx(skill_mtx) -> A

  stopifnot(
    "'agg.method' must be one of the following methods: 'concave', 'linear', 'convex'." = any(
      agg.method[[1]] == c("concave", "linear", "convex")
    )
  )

  stopifnot(
    "'util.fn' must be an utility function or a list of utility functions compatible with 'pref_set'." = any(
      util.fn |> is.function(),
      all(
        util.fn |> is.list(),
        util.fn |> vapply(is.function, logical(1)) |> all(),
        util.fn |> length() == ncol(Uk)
      )
    )
  )

  # multiple dispatch
  return(
    agg.method[[1]] |>
      switch(
        "concave" = "action",
        "linear" = "action",
        "convex" = "action"
      )
  )
}

# endregion
# region: tests
agg.utility(cbind(rep(1, 19), rep(1, 19)), rep(1, 19), util.fn = function(a) a)

# endregion
# # region: exports
# box::export(agg.utility)

# # endregion
