box::use(mod / utils / proper_list[...])
nestmap <- function(x, fn, ...) {
  if (is.proper.list(x)) {
    return(lapply(x, fn, ...))
  }
  return(fn(x, ...))
}

# nestmap <- function(l, fn) {
#   return(
#     l |>
#       as.relistable() |>
#       unlist() |>
#       as.list() |>
#       sapply(fn) |>
#       relist()
#   )
# }
