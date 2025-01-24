# region: imports
box::use(
  assert = mod / utils / assert,
  dom = mod / compare / dom,
  bvls[bvls],
  dplyr[bind_rows, select],
  stats[setNames]
)

# endregion
# region: human capital microflexibility (lowercase phi)
microflex <- function(skill_mtx, weights = NULL, skill.names = NULL) {
  # assert args
  skill_mtx |>
    assert$as.skill_mtx() |>
    t() |>
    as.data.frame() |>
    setNames(skill.names) ->
  skill_mtx

  stopifnot(
    "'weights' must be either NULL or a non-negative numeric vector the same length as the number of rows in 'skill_mtx'." = any(
      all(
        is.numeric(weights),
        length(weights) == nrow(skill_mtx),
        weights >= 0
      ),
      is.null(weights)
    )
  )

  if (is.null(weights)) {
    rep(1, nrow(skill_mtx)) -> weights
  }

  # apply weights
  skill_mtx * sqrt(weights) -> skill_mtx

  # b vectors
  skill_mtx |> as.data.frame() -> attributes

  # A matrix
  skill_mtx |> as.matrix() -> skill_mtx

  # microflex bounds
  scale_lb <- rep(0, ncol(attributes) - 1)
  scale_ub <- rep(1, ncol(attributes) - 1)

  # estimate microflex by means of weighted bvls
  Map(
    function(a_ik, drop) {
      skill_mtx[, -drop] -> A

      bvls(
        A = A,
        b = a_ik,
        bl = scale_lb,
        bu = scale_ub
      )$x -> microflex_coef

      colnames(A) ->
      names(microflex_coef)

      return(microflex_coef)
    },
    a_ik = attributes,
    drop = 1:length(attributes)
  ) |>
    bind_rows() |>
    select(
      names(attributes)
    ) |>
    as.matrix() ->
  microflex_mtx

  diag(microflex_mtx) <- 1

  colnames(microflex_mtx) ->
  rownames(microflex_mtx)

  # human capital microflexibility matrix
  return(microflex_mtx)
}

# endregion
# region: leverage

# endregion
# region: attribute dominance
attribute.dominance <- function(skill_mtx, weights = NULL, skill.names = NULL, aggregate = T) {
  # estimate capital microflexibility matrix, then pass it to the dominance function
  return(microflex(skill_mtx, weights, skill.names) |> dom$dominance(weights = NULL, aggregate))
}

# endregion
# region: exports
box::export(microflex, attribute.dominance)

# endregion
