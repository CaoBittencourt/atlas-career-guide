box::use(
  dplyr[bind_cols, mutate]
)

cbindmap <- function(list, fn, to = NULL, ...) {
  return(
    list |>
      lapply(fn, ...) |>
      bind_cols() |>
      mutate(
        .before = 1,
        to = to
      )
  )
}
