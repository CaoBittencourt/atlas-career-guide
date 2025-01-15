cosine.similarity <- function(ak, aq) {
  # return(sum(ak * aq) / sqrt(sum(ak^2) * sum(aq^2)))
  return(sum(ak * aq) / sqrt((sum(ak^2)) * (sum(aq^2))))
}