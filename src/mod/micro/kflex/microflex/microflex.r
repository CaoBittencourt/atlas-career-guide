# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  bvls[bvls],
  dplyr[bind_rows, select],
  stats[setNames]
)

# endregion
# dispatch
# region: microflex generic function
microflex <- function(skill_mtx, employment = NULL, skill.names = NULL) {
  # assert args
  skill_mtx |>
    assert$
      models$
      as.skill.set.matrix(
      "skill_mtx"
    ) |>
    t() ->
  skill_mtx

  assert$models$validate.employment(employment, "employment", T)

  if (length(skill.names)) {
    skill.names -> colnames(skill_mtx)
  } else {
    paste0("x", 1:ncol(skill_mtx)) -> colnames(skill_mtx)
  }

  # stopifnot(
  #   "'employment' must be either NULL or a non-negative numeric vector the same length as the number of rows in 'skill_mtx'." = any(
  #     all(
  #       is.numeric(employment),
  #       length(employment) == nrow(skill_mtx),
  #       employment >= 0
  #     ),
  #     is.null(employment)
  #   )
  # )


  if (!length(employment)) {
    rep(1, nrow(skill_mtx)) -> employment
  }


  # apply employment
  skill_mtx * sqrt(employment) -> skill_mtx

  # b vectors
  skill_mtx |> as.data.frame() -> attributes

  # A matrix
  skill_mtx |> as.matrix() -> skill_mtx

  # microflex bounds
  # -1 because attributes are dropped iteratively
  lb <- rep(0, ncol(attributes) - 1)
  ub <- rep(1, ncol(attributes) - 1)

  # estimate microflex by means of weighted bvls
  Map(
    function(a_ik, drop) {
      skill_mtx[, -drop] -> A

      bvls(
        A = A,
        b = a_ik,
        bl = lb,
        bu = ub
      )$x -> microflex_coef

      colnames(A) ->
      names(microflex_coef)

      return(microflex_coef)
    },
    a_ik = attributes,
    drop = 1:ncol(attributes)
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
# exports
# region: exports
box::export(microflex)

# endregion
