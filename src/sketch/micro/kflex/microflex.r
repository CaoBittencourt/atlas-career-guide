# region: imports
box::use(
  assert = mod / utils / assert,
  bvls[bvls],
  dplyr[bind_rows, select]
)

# endregion
# region: human capital microflexibility (lowercase phi)
phi <- function(skill_mtx, weights = NULL) {
  # assert args
  assert$valid_skill_mtx(skill_mtx)

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

  # phi bounds
  scale_lb <- rep(0, ncol(attributes) - 1)
  scale_ub <- rep(1, ncol(attributes) - 1)

  # estimate phi by means of weighted bvls
  Map(
    function(a_ik, drop) {
      skill_mtx[, -drop] -> A

      bvls(
        A = A,
        b = a_ik,
        bl = scale_lb,
        bu = scale_ub
      )$x |>
        setNames(
          colnames(A)
        )
    },
    a_ik = attributes,
    drop = 1:length(attributes)
  ) |>
    bind_rows() |>
    select(
      names(attributes)
    ) |>
    as.matrix() ->
  phi_mtx

  diag(phi_mtx) <- 1

  colnames(phi_mtx) ->
  rownames(phi_mtx)

  # human capital microflexibility matrix
  return(phi_mtx)
}

# endregion
