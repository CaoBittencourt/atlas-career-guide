sublist <- function(l, fn) {
  return(l[(l |> sapply(fn) |> sapply(as.logical))])
}
