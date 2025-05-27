interval <- function(bins, max = NA) {
  bins |> sort() -> bins

  return(
    data.frame(
      binId = seq_along(bins),
      from = bins,
      to = bins[-1] |> c(max |> pmax(bins[length(bins)]))
    )
  )
}
