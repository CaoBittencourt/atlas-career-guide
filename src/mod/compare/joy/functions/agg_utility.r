modular::project.options("atlas")
# region: imports
box::use(
  assert = mod / utils / assert[...],
  mod / compare / joy / functions / ueq[...],
  mod / utils / cbindmap[...],
  mod / utils / conform[...],
  dplyr[bind_rows],
)

# endregion
# region: linear utility aggregation
agg.linear <- function(Uk, A, Ük, util.fn) {
  return("linear")
}

# endregion
# region: convex utility aggregation
agg.convex <- function(Uk, A, Ük, util.fn) {
  return("convex")
}

# endregion
# region: concave utility aggregation
agg.concave <- function(Uk, A, Ük, util.fn) {
  return("concave")
}

# endregion
# region: job satisfaction dispatch function
agg.utility <- function(pref_set, skill_mtx, agg.method = c("linear", "concave", "convex")[[1]], ueq.method = c("linear-logistic", "gene-root", "linear")[[1]], util.fn, bind = T) {
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

  # calculate utility equivalence
  Uk |>
    vapply(
      ueq,
      numeric(nrow(Uk)),
      aeq_method = ueq.method[[1]]
    ) |>
    conform(A) ->
  Ük

  # multiple dispatch
  Uk |> conform(A) -> Uk

  agg.method |>
    setNames(
      agg.method
    ) |>
    lapply(
      switch,
      "linear" = Uk |> cbindmap(agg.linear, names(A), A, Ük, util.fn),
      "concave" = Uk |> cbindmap(agg.concave, names(A), A, Ük, util.fn),
      "convex" = Uk |> cbindmap(agg.convex, names(A), A, Ük, util.fn)
    ) ->
  utility.results

  # bind results into data frame
  if (bind) {
    utility.results |>
      bind_rows(
        .id = "method"
      ) ->
    utility.results
  }

  return(utility.results)
}

# endregion
# region: tests
getOption("atlas.skills_mtx") |>
  readRDS() |>
  dplyr::select(-1) ->
dsds

agg.utility(dsds[1:3], dsds[1:3], agg.method = c("linear", "concave", "convex"), util.fn = function(a) a)

# endregion
# # region: exports
# box::export(agg.utility)

# # endregion
