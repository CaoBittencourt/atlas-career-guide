modular::project.options("atlas")
# region: imports
box::use(
  assert = mod / utils / assert[...],
  mod / compare / joy / functions / ueq[...],
  mod / compare / joy / functions / ugene[...],
  mod / utils / rbindmap[...],
  mod / utils / rbindmap[...],
  mod / utils / conform[...],
  stats[weighted.mean],
  dplyr[bind_rows, as_tibble],
)

# endregion
# region: CES utility aggregation
bin.ces <- function(uk, aq
                    # , ük
) {
  # uk |> ugene() -> ugenek
  uk / sum(uk) -> ũk
  1 / (1 - ugene(uk)) -> es

  # ces utility aggregator
  return(
    sum(
      (ũk^(1 / es)) *
        (aq^((es - 1) / es))
    )^(
      es / (es - 1)
    )
  )
  # sum((ũk^(1 / s)) * (u(uk, aq)^((s - 1) / s)))^(s / (s - 1))

  # # perfect substitutes when s -> Inf
  # # perfect complements when s -> 0

  # # ugene -> 1 => utility generalist => perfect substitutes => s -> inf
  # # ugene -> 0 => utility specialist => perfect complements => s -> 0
  # s := f(ugene) | ugene -> 1 => f(ugene) -> Inf, ugene -> 0 => f(ugene) -> 0
  # 1 / (1 - ugene)
  # f(1) = Inf
  # f(0) = 0

  # U <- (
  #   sum(
  #     (a^(1 / s)) * (x^((s - 1) / s))
  #   )
  # )^(
  #   s / (s - 1)
  # )
}

# endregion
# region: linear utility aggregation
agg.linear <- function(Uk, A, Ük, util.fn) {
  return(
    mapply(
      function(uk, ük) {
        return(
          uk |>
            util.fn(A) |>
            vapply(
              weighted.mean,
              w = ük,
              numeric(1)
            )
        )
      },
      uk = Uk,
      ük = Ük
    ) |>
      as_tibble(
        rownames = "to"
      )
  )
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
    "'util.fn' must be a utility function or a list of utility functions compatible with 'pref_set'." = any(
      util.fn |> is.function(),
      all(
        util.fn |> is.list(),
        util.fn |> vapply(is.function, logical(1)) |> all(),
        util.fn |> length() == ncol(Uk)
      )
    )
  )

  # calculate utility equivalence
  Uk |> lapply(ueq, aeq_method = ueq.method[[1]]) -> Ük

  # multiple dispatch
  Uk |> conform(A) -> Uk

  agg.method |>
    setNames(
      agg.method
    ) |>
    lapply(
      switch,
      "linear" = Uk |> agg.linear(A, Ük, util.fn),
      "concave" = Uk |> rbindmap(agg.concave, A, Ük, util.fn),
      "convex" = Uk |> rbindmap(agg.convex, A, Ük, util.fn)
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

box::use(mod / utils / vmap)
dsds |> vmap$vmap(dsds, bin.ces) -> lalala
agg.utility(dsds[1:2], dsds[1:3], agg.method = c("linear", "concave", "convex"), util.fn = function(uk, ak) uk, bind = F) -> lalala
library(dplyr)
library(tidyr)
lalala |>
  as_tibble(
    rownames = "to"
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "utility"
  ) |>
  group_by(from) |>
  arrange(
    -utility,
    .by_group = T
  ) |>
  group_split()



# endregion
# # region: exports
# box::export(agg.utility)

# # endregion
