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
        "ces" = Uk |> cbindmap(agg.ces, names(A), A, util.fn, ...),
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
        "concave" = Uk |> rbindmap(agg.concave, A, Ük, util.fn, ...),
        "convex" = Uk |> rbindmap(agg.convex, A, Ük, util.fn, ...)
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
