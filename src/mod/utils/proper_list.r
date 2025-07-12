is.proper.list <- function(x) {
  return(all(is.list(x), !is.data.frame(x)))
}
