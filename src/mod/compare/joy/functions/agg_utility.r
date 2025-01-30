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
# # region: CES utility aggregation
# bin.ces <- function(uk, aq) {
#   # uk |> ugene() -> ugenek
#   uk / sum(uk) -> ũk
#   1 / (1 - (1 - ugene(uk))) -> es

#   # ces utility aggregator
#   return(
#     sum(
#       (ũk^(1 / es)) *
#         (aq^((es - 1) / es))
#     )^(
#       es / (es - 1)
#     )
#   )
#   # sum((ũk^(1 / s)) * (u(uk, aq)^((s - 1) / s)))^(s / (s - 1))

#   # # perfect substitutes when s -> Inf
#   # # perfect complements when s -> 0

#   # # ugene -> 1 => utility generalist => perfect substitutes => s -> inf
#   # # ugene -> 0 => utility specialist => perfect complements => s -> 0
#   # s := f(ugene) | ugene -> 1 => f(ugene) -> Inf, ugene -> 0 => f(ugene) -> 0
#   # 1 / (1 - ugene)
#   # f(1) = Inf
#   # f(0) = 0

#   # U <- (
#   #   sum(
#   #     (a^(1 / s)) * (x^((s - 1) / s))
#   #   )
#   # )^(
#   #   s / (s - 1)
#   # )
# }

# # endregion
# region: CES utility aggregation
bin.ces <- function(uk, aq, util.fn = NULL) {
  # uk |> ugene() -> ugenek
  # uk / sum(uk) -> ũk
  1 / (1 - ugene(uk)) -> es

  if (length(util.fn)) {
    util.fn(uk, aq) -> uk
  }
  # 1 + es -> es

  # ces utility aggregator
  return(
    sum(
      ((aq / sum(aq))^(1 / es)) *
        (uk^((es - 1) / es))
      # (ũk^(1 / es)) *
      #   (aq^((es - 1) / es))
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
library(dplyr)
library(tidyr)
box::use(mod / utils / logistic)
getOption("atlas.skills_mtx") |>
  readRDS() |>
  dplyr::select(-1) ->
df_occupations

getOption("atlas.cao") |>
  readRDS() ->
df_cao

getOption("atlas.skills") |>
  readRDS() |>
  inner_join(
    df_cao |> select(item)
  ) |>
  pivot_wider(
    names_from = occupation,
    values_from = item_score
  ) ->
df_occupations_cao

df_cao |>
  agg.utility(
    df_occupations_cao,
    util.fn = function(uk, aq) {
      # 1 - (2 * aq - uk)^2
      # logistic$logistic(aq, 0, 1, 1, 1, uk, 1, 1)
      uk * aq
      # logistic$logistic(aq, 0, uk, 1, 1, uk, 1, 1)
    }
  ) |>
  arrange(desc(cao)) |>
  slice(
    1:10, (ncol(df_occupations_cao) - 10):ncol(df_occupations_cao)
  ) |>
  group_by(row_number() > 10) |>
  group_split(.keep = F)

getOption("atlas.skills") |>
  readRDS() |>
  inner_join(
    df_cao
  ) |>
  group_by(
    occupation
  ) |>
  reframe(
    utility = bin.ces(
      uk = cao,
      # aq = 1 - (2 * item_score - cao)^2
      aq = item_score,
      util.fn = function(uk, aq) {
        # ueq(uk) * aq
        aq^(1 / ueq(uk))
        # logistic$logistic(
        #   x = aq,
        #   a = 0,
        #   k = uk,
        #   c = 1,
        #   q = 1,
        #   m = uk,
        #   b = 1,
        #   nu = 1
        # )
      }
      # aq = item_score * cao
      #   logistic$logistic(
      #   x = item_score,
      #   a = 0,
      #   k = 1,
      #   c = 1,
      #   q = 1,
      #   m = cao,
      #   b = 1 / (1 - ugene(cao)),
      #   nu = 1
      # )
      # 1 - (2 * item_score - cao)^2
      # aq = item_score
    )
  ) |>
  mutate(
    utility.norm =
      utility / max(
        utility
      )
  ) |>
  arrange(desc(
    utility
  )) |>
  print(n = 100)


# box::use(mod / utils / vmap)
# df_occupations |> vmap$vmap(df_occupations, bin.ces) -> lalala
# agg.utility(df_occupations[1:2], df_occupations[1:3], agg.method = c("linear", "concave", "convex"), util.fn = function(uk, ak) uk, bind = F) -> lalala
# library(dplyr)
# library(tidyr)
# lalala |>
#   as_tibble(
#     rownames = "to"
#   ) |>
#   pivot_longer(
#     cols = -1,
#     names_to = "from",
#     values_to = "utility"
#   ) |>
#   group_by(from) |>
#   arrange(
#     -utility,
#     .by_group = T
#   ) |>
#   group_split()

# endregion
# # region: exports
# box::export(agg.utility)

# # endregion
