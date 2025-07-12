box::use(
  dplyr[...]
)

# kmeans clusters to grid
as.grid <- function(kmeans, types = NULL) {
  # assert args
  stopifnot(
    "'kmeans' must be a 'kmeans' object." =
      class(kmeans) == "kmeans"
  )

  if (is.null(types)) {
    kmeans$
      centers |>
      seq_along() ->
    types
  }

  return(
    tibble(
      var = kmeans$centers |> as.numeric(),
      pct = kmeans$size / sum(kmeans$size)
    ) |>
      arrange(var) |>
      mutate(
        .before = 1,
        type = types |> as.factor()
      )
  )
}
