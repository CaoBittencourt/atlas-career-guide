# setup
# region: imports
box::use(
  types = mod / utils / assert / base / types[...],
  utils = mod / utils / assert / utils[...]
)

# endregion
# type asserts
# generic types
# region: proper list type
validate.proper.list <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.proper.list,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "a proper list."
      )
  )
}

# endregion
# region: matrix-like type
validate.matrix.like <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.matrix.like,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "a data frame or matrix."
      )
  )
}

# endregion
# numeric types
# region: basic numeric type
validate.numeric <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.numeric.vector,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "numeric."
      )
  )
}

# endregion
# region: unit type
validate.unit <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.unit,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "numeric and in the unit interval."
      )
  )
}

# endregion
# region: signed unit type
validate.unit.signed <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.unit.signed,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "numeric with absolute value in the unit interval."
      )
  )
}

# endregion
# region: bernoulli type
validate.bernoulli <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.bernoulli,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "either 0 or 1."
      )
  )
}

# endregion
# numeric matrix types
# region: unit matrix type
validate.unit.matrix <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.unit.matrix,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "a numeric matrix in the unit interval."
      )
  )
}

# endregion
# region: signed unit matrix type
validate.unit.signed.matrix <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.unit.signed.matrix,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "a numeric matrix with absolute value in the unit interval."
      )
  )
}

# endregion
# region: bernoulli matrix type
validate.bernoulli.matrix <- function(x, arg.name = NULL, nullable = F) {
  return(
    x |>
      utils$validate(
        type = types$is.bernoulli.matrix,
        nullable = nullable,
        arg.name = arg.name,
        type.def = "a numeric matrix where all values are either 0 or 1."
      )
  )
}

# endregion
# misc asserts
# region: assert methods
validate.method <- function(x, arg.name = NULL, methods) {
  if (!any(x %in% methods)) {
    stop(
      paste0(
        "'", arg.name, "'",
        " must be one of following methods: ",
        methods |>
          sapply(
            function(x) {
              paste0('"', x, '"')
            }
          ) |>
          paste0(
            collapse = ", "
          ),
        "."
      )
    )
  }
}

# endregion
# export
# region: exports
box::export(
  # generic types
  validate.proper.list,
  validate.matrix.like,
  # numeric types
  validate.numeric,
  validate.unit,
  validate.unit.signed,
  validate.bernoulli,
  # numeric matrix types
  validate.unit.matrix,
  validate.unit.signed.matrix,
  validate.bernoulli.matrix,
  # misc
  validate.method
)

# endregion
