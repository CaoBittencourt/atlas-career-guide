cosine.similarity <- function(ak, aq) {
  return(ak * aq / (sum(ak^2) * sum(aq^2)))
}
