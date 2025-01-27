modular::project.options("atlas")
# region: imports
box::use(
  assert = mod / utils / assert[...],
  mod / compare / joy / functions / ueq[...]
)

# endregion
# region: concave utility aggregation

# endregion
# region: linear utility aggregation
agg.linear <- function(Uk, Ak, Ük, util.fn) {

}

# endregion
# region: convex utility aggregation

# endregion
# region: job satisfaction dispatch function
agg.utility <- function(pref_set, skill_mtx, agg.method = c("linear", "concave", "convex")[[1]], ueq.method = c("linear-logistic", "gene-root", "linear")[[1]], util.fn) {
  # assert args
  assert$as.skill_mtx(pref_set) -> pref_set
  assert$as.skill_mtx(skill_mtx) -> skill_mtx

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
        util.fn |> length() == ncol(pref_set)
      )
    )
  )

  # calculate utility equivalence
  pref_set |>
    vapply(
      ueq,
      numeric(nrow(pref_set)),
      aeq_method = ueq.method[[1]]
    ) |>
    as.data.frame() ->
  ueq_set

  return(ueq_set)

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
getOption("atlas.skills_mtx") |>
  readRDS() |>
  dplyr::select(2:3) ->
dsds
agg.utility(dsds, rep(1, 19), ueq.method = "gene-root", util.fn = function(a) a) |> summary()

# endregion
# # region: exports
# box::export(agg.utility)

# # endregion
