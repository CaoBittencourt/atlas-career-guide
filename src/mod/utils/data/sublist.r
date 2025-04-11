sublist <- function(l, fn, ..., negate = F) {
  if (negate) {
    return(l[!(l |> sapply(fn) |> sapply(as.logical))])
  }
  return(l[(l |> sapply(fn) |> sapply(as.logical))])
}
