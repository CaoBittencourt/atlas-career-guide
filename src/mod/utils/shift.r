min.shift <- function(x) {
  x - min(x, na.rm = T) -> x
  x / max(x, na.rm = T) -> x
  return(x)
}

gene.shift <- function(x) {
  # assert args in generality function
  # gene shift, then min shift
  return(min.shift(x - gn$gene(x)))
}
