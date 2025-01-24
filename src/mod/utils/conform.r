# region: imports
box::use(
  tidyr[as_tibble],
  stats[setNames]
)

# endregion
# region: regular conform
conform.x <- function(x, A) {
  x |> as.data.frame() -> x
  A |> as.data.frame() -> A

  rep(1, ncol(A)) -> reps
  names(A) -> mtx.names

  if (!length(mtx.names)) {
    "x" |>
      paste0(
        seq_along(reps)
      ) -> mtx.names
  }

  return(
    x |>
      as.list() |>
      lapply(cbind) |>
      lapply(
        function(mtx) {
          mtx[, reps] |>
            cbind() ->
          mtx

          mtx.names ->
          colnames(mtx)

          return(mtx)
        }
      ) |>
      lapply(
        as_tibble,
        .name_repair =
          "minimal"
      )
  )
}

# endregion
# region: replicate conform
conform.rep <- function(x, A) {
  x |> as.data.frame() -> x
  A |> as.data.frame() -> A

  rep(1, ncol(A)) -> reps

  rep(1, ncol(A)) -> reps
  names(A) -> mtx.names

  if (!length(mtx.names)) {
    "x" |>
      paste0(
        seq_along(reps)
      ) -> mtx.names
  }


  return(
    list(
      from =
        x |>
          as.list() |>
          lapply(cbind) |>
          lapply(
            function(mtx) {
              mtx[, reps] |>
                cbind() ->
              mtx

              mtx.names ->
              colnames(mtx)

              return(mtx)
            }
          ) |>
          lapply(
            as_tibble,
            .name_repair =
              "minimal"
          ),
      to =
        x |>
          ncol() |>
          replicate(
            A,
            simplify = F
          )
    )
  )
}

# endregion
# region: generic function
conform <- function(x, A, rep = F) {
  # multiple dispatch
  if (rep) {
    return(conform.rep(x, A))
  }

  return(conform.x(x, A))
}

# endregion
# region: exports
box::export(conform)

# endregion
