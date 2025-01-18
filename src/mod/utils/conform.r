conform <- function(x, A) {
  x |> as.data.frame() -> x
  A |> as.data.frame() -> A

  rep(1, ncol(A)) -> reps

  x |>
    as.list() |>
    lapply(cbind) |>
    lapply(function(mtx) {
      mtx[, reps]
    })
}
