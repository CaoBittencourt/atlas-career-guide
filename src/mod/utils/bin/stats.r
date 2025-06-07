box::use(
  mod / utils / bin / sample[...],
  stats[sd],
)

mean.kde <- function(kde, ...) {
  return(
    kde |> sample.kde(...) |> mean()
  )
}

sd.kde <- function(kde, ...) {
  return(
    kde |> sample.kde(...) |> sd()
  )
}
