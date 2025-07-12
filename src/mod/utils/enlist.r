box::use(utils / proper_list[...])
enlist <- function(x) {
  if (is.proper.list(x)) {
    return(x)
  }
  return(list(x))
}
