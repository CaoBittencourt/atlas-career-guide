# region: imports
box::use(
  weights[wtd.cors]
)

# endregion
# region: pearson correlation matching method
s.pearson <- function(ak, aq, äq) {
  # assert args in main function
  return((1 + wtd.cors(ak, aq, äq)) / 2)
}

# endregion
# region: exports
box::export(s.pearson)

# endregion
