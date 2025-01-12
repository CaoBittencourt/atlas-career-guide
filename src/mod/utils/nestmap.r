nestmap <- function(l, fn) {
  return(
    l |>
      as.relistable() |>
      unlist() |>
      as.list() |>
      sapply(fn) |>
      relist()
  )
}
