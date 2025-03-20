# setup
# region: imports
box::use(
  assert = mod / utils / assert / base / validate[...],
  dplyr[select]
)

# endregion
# numeric matrix types
# region: unit matrix type
as.unit.matrix <- function(x, arg.name = NULL) {
  x |>
    as.data.frame() |>
    select(
      where(is.numeric)
    ) |>
    as.matrix() ->
  x

  assert$validate.unit.matrix(x, arg.name, nullable = F)
  return(x)
}

# endregion
# region: unit signed matrix type
as.unit.signed.matrix <- function(x, arg.name = NULL) {
  x |>
    as.data.frame() |>
    select(
      where(is.numeric)
    ) |>
    as.matrix() ->
  x

  assert$validate.unit.signed.matrix(x, arg.name, nullable = F)
  return(x)
}

# endregion
# exports
# region: exports
box::export(
  as.unit.matrix,
  as.unit.signed.matrix
)

# endregion
