# nullable type
nullable <- function(x) {
  return(any(x, is.null(x)))
}
