modular::project.options("atlas")
# region: imports
box::use(
  assert = mod / utils / assert[...],
  mod / compare / joy / ueq[...],
  mod / compare / joy / ugene[...],
  mod / utils / rbindmap[...],
  mod / utils / cbindmap[...],
  mod / utils / conform[...],
  stats[weighted.mean],
  dplyr[bind_rows, as_tibble],
)

# endregion
# # region: CES utility aggregation
# bin.ces <- function(uk, aq, util.fn = NULL, ...) {
#   # elasticity of substitution
#   # perfect substitutes when es -> Inf
#   # perfect complements when es -> 0
#   # perfect substitutes <=> utility generalist <=> ugene = 1
#   # perfect complements <=> utility specialist <=> ugene = 0
#   1 / (1 - ugene(uk)) -> es

#   # apply utility function
#   if (length(util.fn)) {
#     util.fn(uk, aq, ...) -> uk
#   }

#   # ces utility aggregator
#   return(
#     sum(
#       ((aq / sum(aq))^(1 / es)) *
#         (aq * uk^((es - 1) / es))
#     )^(
#       es / (es - 1)
#     )
#   )
# }

# # endregion
# region: CES utility aggregation
agg.ces <- function(Uk, A, util.fn = NULL, ...) {
  # elasticity of substitution
  # perfect substitutes when es -> Inf
  # perfect complements when es -> 0
  # perfect substitutes <=> utility generalist <=> ugene = 1
  # perfect complements <=> utility specialist <=> ugene = 0
  1 / (1 - ugene(Uk[[1]])) -> es

  # apply utility function
  util.fn |> mapply(Uk, A, ...) -> Uk

  # ces utility aggregator
  return(
    colSums(
      ((A / colSums(A))^(1 / es)) *
        (A * Uk^((es - 1) / es))
    )^(
      es / (es - 1)
    )
  )
}

# endregion
# # region: linear utility aggregation
# agg.linear <- function(Uk, A, Ük, util.fn, ...) {
#   return(
#     Map(
#       function(uk, ük, aq) {
#         return(
#           weighted.mean(
#             do.call(util.fn, list(uk,aq))
#             # util.fn(uk, aq, ...),
#             w = ük
#           )
#         )
#       },
#       uk = Uk,
#       ük = Ük,
#       aq = A
#     ) |>
#       as_tibble(
#         rownames = "to"
#       )
#   )
# }

# # endregion
# # region: linear utility aggregation
# agg.linear <- function(Uk, A, Ük, util.fn, ...) {
#   return(
#     mapply(
#       function(uk, ük) {
#         return(
#           uk |>
#             util.fn(A, ...) |>
#             vapply(
#               weighted.mean,
#               w = ük,
#               numeric(1)
#             )
#         )
#       },
#       uk = Uk,
#       ük = Ük
#     ) |>
#       as_tibble(
#         rownames = "to"
#       )
#   )
# }

# # endregion
# region: convex utility aggregation
agg.convex <- function(Uk, A, Ük, util.fn, ...) {
  return("convex")
}

# endregion
# region: concave utility aggregation
agg.concave <- function(Uk, A, Ük, util.fn, ...) {
  return("concave")
}

# endregion
# region: job satisfaction dispatch function
agg.utility <- function(pref_set, skill_mtx, agg.method = c("ces", "linear", "concave", "convex")[[1]], ueq.method = c("linear-logistic", "gene-root", "linear")[[1]], util.fn, ..., bind = T) {
  # assert args
  assert$as.skill_mtx(pref_set) -> Uk
  assert$as.skill_mtx(skill_mtx) -> A

  stopifnot(
    "'agg.method' must be one of the following methods: 'ces', 'linear', 'concave', 'convex'." = all(
      agg.method %in% c("ces", "linear", "concave", "convex")
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
  if (any(agg.method %in% c("linear", "concave", "convex"))) {
    Uk |> lapply(ueq, aeq_method = ueq.method[[1]]) -> Ük
  }

  # multiple dispatch
  Uk |> conform(A) -> Uk

  return(list(
    Uk = Uk,
    Ük = Ük,
    A = A
  ))

  agg.method |>
    setNames(
      agg.method
    ) |>
    lapply(
      switch,
      "ces" = Uk |> cbindmap(agg.ces, names(A), A, util.fn, ...),
      "linear" = Map(
        function(U, Ü) {
          agg.linear(U, A, Ü, util.fn)
        },
        U = Uk,
        Ü = Ük,
        ...
      ),
      "concave" = Uk |> rbindmap(agg.concave, A, Ük, util.fn, ...),
      "convex" = Uk |> rbindmap(agg.convex, A, Ük, util.fn, ...)
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
box::use(
  mod / utils / logistic,
  dplyr[...], ,
  tidyr[...],
)

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
    agg.method = "linear",
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

# getOption("atlas.skills") |>
#   readRDS() |>
#   inner_join(
#     df_cao
#   ) |>
#   group_by(
#     occupation
#   ) |>
#   reframe(
#     utility = bin.ces(
#       uk = cao,
#       aq = item_score,
#       util.fn = function(uk, aq) {
#         # uk
#         # aq
#         # ueq(uk)
#         # ueq(aq)
#         # ueq(uk) * ueq(aq)
#         # ueq(aq)^(1 / ueq(uk))
#         # uk * aq
#         ueq(uk) * aq
#         # aq^(1 / ueq(uk))
#         # aq^(1 / uk)

#         # logistic$logistic(
#         #   x = aq,
#         #   a = 0,
#         #   k = uk,
#         #   c = 1,
#         #   q = 1,
#         #   m = uk,
#         #   b = 1,
#         #   nu = 1
#         # )
#       }
#     )
#   ) |>
#   mutate(
#     utility.norm =
#       utility / max(
#         utility
#       )
#   ) |>
#   arrange(desc(
#     utility
#   )) |>
#   print(n = 100)


# box::use(mod / utils / vmap[...])
# df_occupations[1:3] |>
#   vmap(
#     df_occupations,
#     bin.ces
#   ) |>
#   as_tibble(
#     rownames = "to"
#   )

# df_occupations[1:3] |>
df_cao |>
  agg.utility(
    # df_occupations,
    df_occupations_cao,
    agg.method = c("ces", "linear"),
    util.fn = function(uk, aq) {
      ueq(uk) * aq
    }
  )

df_occupations[1:3] |>
  agg.utility(
    df_occupations,
    agg.method = "linear",
    util.fn = function(uk, aq) {
      uk * aq
    }
  )

df_occupations[1:2] |>
  agg.utility(
    df_occupations[1:3],
    agg.method = "linear",
    util.fn = function(uk, aq) {
      uk * aq
    }
  ) -> dsds

# Map(
#   function(uk, ük, aq) {
#     return(
#       do.call(
#         function(uk, aq) {
#           weighted.mean(
#             ueq(uk) * aq,
#             w = ük
#           )
#         },
#         args = list(uk, aq)
#       )
#     )
#   },
#   uk = dsds$`Accountants and Auditors`,
#   ük = dsds$`Accountants and Auditors`,
#   aq = df_occupations[1:3]
# ) |>
#   as_tibble(
#     rownames = "to"
#   )

agg.linear <- function(Uk, A, Ük, util.fn, ...) {
  return(
    Map(
      function(uk, ük, aq) {
        return(list(
          uk = uk,
          ük = ük,
          aq = aq
        ))
      },
      uk = Uk,
      ük = Ük,
      aq = A
    )
  )
}

agg.linear(dsds$Uk, dsds$A, dsds$Ük, function(uk, aq) uk * aq)


# endregion
# region: exports
box::export(agg.utility)

# endregion
