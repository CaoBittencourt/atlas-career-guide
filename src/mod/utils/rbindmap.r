box::use(
  dplyr[bind_rows]
)

rbindmap <- function(list, fn, ...) {
  return(
    list |>
      lapply(fn, ...) |>
      bind_rows(
        .id = "to"
      )
  )
}
