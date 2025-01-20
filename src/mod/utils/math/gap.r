gap <- function(ak, aq) {
  diff <- ak - aq
  diff[diff < 0] <- 0
  return(diff)
}
