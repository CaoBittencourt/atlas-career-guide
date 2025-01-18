# region: imports

# endregion
# region: euclidean matching method
euclidean <- function(ak, aq, äq = rep(1, length(aq))) {
  # assert args in main function
  # calculate normalized weighted euclidean distance
  return(1 - sqrt(sum(äq * (ak - aq)^2)) / sqrt(sum(äq * pmax(1 - aq, aq - 0)^2)))
}

# endregion
# region: exports
box::export(euclidean)

# endregion
