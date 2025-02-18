# region: imports
box::use(
  assert = mod / utils / assert[...],
  mod / compare / joy / ueq[...],
  mod / compare / joy / ugene[...],
  mod / utils / rbindmap[...],
  mod / utils / cbindmap[...],
  mod / utils / conform[...],
  stats[weighted.mean],
  dplyr[bind_rows, as_tibble, mutate],
)

# endregion
# region: CES utility aggregation
agg.ces <- function(uk, A, util.fn = NULL, ...) {
  # elasticity of substitution (es)

  # perfect substitutes when es -> Inf
  # perfect complements when es -> 0

  # perfect substitutes <=> utility generalist <=> ugene = 1
  # perfect complements <=> utility specialist <=> ugene = 0

  # therefore,
  # ugene -> 1 <=> rho -> 1 <=> perfect substitutes
  # ugene -> 0 <=> rho -> Inf <=> perfect complements
  # ugene -> 0.5 <=> rho -> 0 <=> cobb-douglas

  # the transformation function from ugene to rho must not be negative:
  # (0.5 - upsilon.gamma) / (-0.5 * upsilon.gamma) -> rho
  # (2 - (1 / upsilon.gamma)) -> rho
  # ((upsilon.gamma - (1 / 2))^3) / (upsilon.gamma / 8) -> rho
  # log(2 * upsilon.gamma) * exp(
  #   upsilon.gamma / (
  #     1 / log(
  #       1 / log(2)
  #     )
  #   )
  # ) -> rho


  # preference generality to elasticity of substitution mapper
  # midpoint = 0.5 <=> cobb-douglas
  ugene(uk) -> upsilon.gamma
  (4 / upsilon.gamma) * (upsilon.gamma - 0.5)^2 -> rho

  # ces utility aggregator
  return(
    A |>
      sapply(
        function(aq) {
          sum(
            (aq / sum(aq)) * (
              mapply(
                util.fn,
                uk,
                aq
              )^rho
            )
          )^(1 / rho)
        }
      )
  )
}

# endregion
# # region: CES utility aggregation
# agg.ces <- function(Uk, A, util.fn = NULL, ...) {
#   # elasticity of substitution (es)

#   # perfect substitutes when es -> Inf
#   # perfect complements when es -> 0

#   # perfect substitutes <=> utility generalist <=> ugene = 1
#   # perfect complements <=> utility specialist <=> ugene = 0

#   # therefore,
#   # ugene -> 1 <=> rho -> 1 <=> perfect substitutes
#   # ugene -> 0 <=> rho -> Inf <=> perfect complements
#   # ugene -> 0.5 <=> rho -> 0 <=> cobb-douglas

#   # the transformation function from ugene to rho must not be negative:
#   # (0.5 - upsilon.gamma) / (-0.5 * upsilon.gamma) -> rho
#   # (2 - (1 / upsilon.gamma)) -> rho
#   # ((upsilon.gamma - (1 / 2))^3) / (upsilon.gamma / 8) -> rho
#   # log(2 * upsilon.gamma) * exp(
#   #   upsilon.gamma / (
#   #     1 / log(
#   #       1 / log(2)
#   #     )
#   #   )
#   # ) -> rho


#   # preference generality to elasticity of substitution mapper
#   # midpoint = 0.5 <=> cobb-douglas
#   ugene(Uk[[1]]) -> upsilon.gamma
#   (4 / upsilon.gamma) * (upsilon.gamma - 0.5)^2 -> rho

#   # ces utility aggregator
#   return(
#     A |>
#       sapply(
#         function(aq) {
#           sum(
#             (aq / sum(aq)) * (
#               mapply(
#                 util.fn,
#                 Uk[[1]],
#                 aq
#               )^rho
#             )
#           )^(1 / rho)
#         }
#       )
#     # A |>
#     #   vapply(
#     #     function(aq) {
#     #       sum(
#     #         (aq / sum(aq)) * (
#     #           # apply utility function
#     #           mapply(
#     #             util.fn,
#     #             Uk[[1]],
#     #             aq,
#     #             ...
#     #           )^rho
#     #         )
#     #       )^(1 / rho)
#     #     },
#     #     numeric(1)
#     #   )
#   )
# }

# # endregion
# # region: CES utility aggregation
# agg.ces <- function(Uk, A, util.fn = NULL, ...) {
#   # elasticity of substitution

#   # perfect substitutes when es -> Inf
#   # perfect complements when es -> 0

#   # perfect substitutes <=> utility generalist <=> ugene = 1
#   # perfect complements <=> utility specialist <=> ugene = 0

#   # therefore,
#   # ugene -> 1 <=> rho -> 1 <=> perfect substitutes
#   # ugene -> 0 <=> rho -> Inf <=> perfect complements
#   # ugene -> 0.5 <=> rho -> 0 <=> cobb-douglas
#   ugene(Uk[[1]]) -> upsilon.gamma
#   (4 / upsilon.gamma) * (upsilon.gamma - 0.5)^2 -> rho
#   # (1 / upsilon.gamma) -> rho

#   # note: the transformation function from ugene to rho must not be negative
#   # (0.5 - upsilon.gamma) / (-0.5 * upsilon.gamma) -> rho
#   # (2 - (1 / upsilon.gamma)) -> rho
#   # ((upsilon.gamma - (1 / 2))^3) / (upsilon.gamma / 8) -> rho
#   # log(2 * upsilon.gamma) * exp(
#   #   upsilon.gamma / (
#   #     1 / log(
#   #       1 / log(2)
#   #     )
#   #   )
#   # ) -> rho

#   # apply utility function
#   util.fn |> mapply(Uk, A, ...) -> Uk

#   # ces utility aggregator
#   return(
#     colSums(
#       (A / colSums(A)) * (Uk^rho)
#     )^(1 / rho)
#   )
# }

# # endregion
# region: linear utility aggregation
agg.linear <- function(Uk, A, ük, util.fn, ...) {
  return(
    mapply(
      function(uk, aq) {
        return(
          uk |>
            util.fn(aq, ...) |>
            weighted.mean(ük)
        )
      },
      uk = Uk,
      aq = A
    )
  )
}

# endregion
# region: convex utility aggregation
agg.convex <- function(Uk, A, ük, util.fn, ...) {
  # ugene for constant convex exponents
  ugene.root <- 1 / ugene(Uk[[1]])

  return(
    mapply(
      function(uk, aq) {
        return(
          (
            util.fn(uk, aq, ...)^ugene.root
          ) |>
            weighted.mean(ük)
        )
        # return(
        #   sum((
        #     util.fn(uk, aq, ...)
        #   ) * ük)
        # )
      },
      uk = Uk,
      aq = A
    )
  )
}

# endregion
# region: concave utility aggregation
agg.concave <- function(Uk, A, ük, util.fn, ...) {
  # assert args in main function

  # apply utility function
  # get the geometric mean
  üroot <- 1 / sum(ük)
  return(
    mapply(
      function(uk, aq) {
        return(
          prod(
            util.fn(uk, aq, ...)^ük
          )^üroot
        )
      },
      uk = Uk,
      aq = A
    )
  )
}

# endregion
# region: job satisfaction dispatch function
agg.utility <- function(pref_set, skill_mtx, agg.method = c("ces", "linear", "concave", "convex")[[1]], ueq.method = c("linear-logistic", "gene-root", "linear")[[1]], util.fn, ...) {
  # assert args
  assert$as.skill_mtx(pref_set) -> Uk
  assert$as.skill_mtx(skill_mtx) -> A

  stopifnot(
    "'agg.method' must be one of the following methods: 'ces', 'linear', 'concave', 'convex'." = any(
      agg.method[[1]] == c("ces", "linear", "concave", "convex")
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
  if (any(agg.method[[1]] == c("linear", "concave", "convex"))) {
    Uk |> lapply(ueq, aeq_method = ueq.method[[1]]) -> Ük
  }

  # multiple dispatch
  Uk |> conform(A) -> Uk

  return(
    agg.method[[1]] |>
      switch(
        "ces" = mapply(
          function(uk) {
            agg.ces(uk, A, util.fn, ...)
          },
          uk = Uk |>
            lapply(function(x) {
              x[[1]]
            })
        ) |>
          as_tibble(
            rownames = "to"
          ),
        # "ces" = Uk |> cbindmap(agg.ces, names(A), A, util.fn, ...),
        "linear" = mapply(
          function(U, ü) {
            agg.linear(U, A, ü, util.fn, ...)
          },
          Uk,
          Ük
        ) |>
          as_tibble(
            rownames = "to"
          ),
        "concave" = mapply(
          function(U, ü) {
            agg.concave(U, A, ü, util.fn, ...)
          },
          Uk,
          Ük
        ) |>
          as_tibble(
            rownames = "to"
          ),
        "convex" = mapply(
          function(U, ü) {
            agg.convex(U, A, ü, util.fn, ...)
          },
          Uk,
          Ük
        ) |>
          as_tibble(
            rownames = "to"
          ),
      ) |>
      mutate(
        .before = 1,
        method = agg.method[[1]]
      )
  )
}

# endregion
# region: exports
box::export(agg.utility)

# endregion
# # region: test
# df_cao$cao^1 / (1 - df_cao$cao)
# df_cao$cao^((1 - df_cao$cao) / df_cao$cao)


# # endregion
