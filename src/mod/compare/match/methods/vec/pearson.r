# region: imports
box::use(
  weights[wtd.cors],
  dplyr[bind_rows]
)

# endregion
# region: pearson correlation matching method
pearson <- function(Ak, A, Ä) {
  # assert args in main function
  # lapply weighted pearson correlation
  return(
    Ak |>
      lapply(
        function(ak) {
          wtd.cors |>
            mapply(
              ak,
              A,
              Ä
            )
        }
      ) |>
      lapply(
        function(s) {
          (1 + s) / 2
        }
      ) |>
      bind_rows(
        .id = "to"
      )
  )
}

# endregion
# region: exports
box::export(pearson)

# endregion
