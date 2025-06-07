# sample from a kernel density estimation
sample.kde <- function(kde, n = length(kde$x)) {
  return(
    kde$x |>
      sample(
        size = n,
        replace = T,
        prob = kde$y
      )
  )
}
