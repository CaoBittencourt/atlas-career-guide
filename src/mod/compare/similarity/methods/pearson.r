# region: imports
box::use(
  weights[wtd.cors]
)

# endregion
# region: pearson correlation matching method
similarity.pearson <- function(ak, A, Ä) {
  # assert args in main function
  # weighted pearson correlation
  return(
    mapply(
      function(k, q, w) {
        (1 + wtd.cors(k, q, w)) / 2
      },
      ak,
      A,
      Ä
    )
  )
}

# endregion
# region: exports
box::export(similarity.pearson)

# endregion
