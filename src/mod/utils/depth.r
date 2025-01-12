# recursive list depth
depth <- function(x) {
  if (is.list(x)) {
    return(x |> sapply(depth) |> max() + 1)
  }
  return(0)
}
