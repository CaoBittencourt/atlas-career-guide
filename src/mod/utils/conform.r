box::use(
  tidyr[as_tibble],
  stats[setNames]
)

conform <- function(x, A) {
  x |> as.data.frame() -> x
  A |> as.data.frame() -> A

  rep(1, ncol(A)) -> reps

  return(
    x |>
      as.list() |>
      lapply(cbind) |>
      lapply(
        function(mtx) {
          mtx[, reps] -> mtx

          "x" |>
            paste0(
              seq_along(reps)
            ) ->
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
