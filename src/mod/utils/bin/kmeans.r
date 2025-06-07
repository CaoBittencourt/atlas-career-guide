box::use(
  assert = mod / utils / assert,
  stats[kmeans],
  mod / utils / bin / sample[...],
)

# kmeans clusters from kde
kmeans.kde <- function(kde, k, n = NULL, ...) {
  # assert args
  k |> assert$base$validate.numeric.bounded("k", F, 1)
  n |> assert$base$validate.numeric.bounded("n", T, 1)
  stopifnot(
    "'kde' must be a 'density' object." =
      class(kde) == "density"
  )
  stopifnot(
    "'k' must be an integer." = round(k) == k
  )
  stopifnot(
    "'n' must be either NULL or an integer." = round(k) == k
  )

  if (is.null(n)) {
    kde$n -> n
  }

  return(
    kde |>
      sample.kde(n) |>
      kmeans(k, ...)
  )
}


box::export(kmeans.kde)
