# setup
# region: imports
box::use(
  types = mod / utils / assert / base / types[...],
  utils = mod / utils / assert / utils[...]
)

# endregion
# type asserts
# generic types
# region: proper list
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

#endregion
# numeric types
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
  # numeric types
  validate.unit,
  validate.unit.signed,
  # numeric matrix types
  validate.unit.matrix,
  validate.unit.signed.matrix,
  # misc
  validate.method
)

# endregion
