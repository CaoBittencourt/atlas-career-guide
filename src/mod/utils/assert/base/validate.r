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
validate.proper.list <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.proper.list,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "a proper list."
        },
        ...
      )
  )
}

# endregion
# region: boolean
validate.bool <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.bool,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "TRUE or FALSE."
        },
        ...
      )
  )
}

# endregion
# numeric types
# region: basic numeric
validate.numeric <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.numeric.vector,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "numeric."
        },
        ...
      )
  )
}

# endregion
# region: unit
validate.unit <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.unit,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "numeric and in the unit interval."
        },
        ...
      )
  )
}

# endregion
# region: signed unit
validate.unit.signed <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.unit.signed,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "numeric with absolute value in the unit interval."
        },
        ...
      )
  )
}

# endregion
# region: bernoulli
validate.bernoulli <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.bernoulli,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "either 0 or 1."
        },
        ...
      )
  )
}

# endregion
# region: bounded numeric
validate.numeric.bounded <- function(x, arg.name = NULL, nullable = F, lb = NULL, ub = NULL, lc = T, rc = T, ...) {
  return(
    x |>
      utils$validate(
        type = function(x) {
          types$is.numeric.bounded(x, lb, ub, lc, rc)
        },
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          msg <- "numeric"

          if (length(lb) & !length(ub)) {
            msg |>
              paste(
                "and greater than",
                ifelse(
                    lc,
                    "or equal to",
                    ""
                ),
                lb
              ) -> msg
          }

          if (!length(lb) & length(ub)) {
            msg |>
              paste(
                "and less than",
                ifelse(
                    rc,
                    "or equal to",
                    ""
                ),
                ub
              ) -> msg
          }

          if (length(lb) & length(ub)) {
            msg |>
              paste(
                "and between",
                lb,
                ifelse(
                  lc,
                  "(inclusive)",
                  "(exclusive)"
                ),
                "and",
                ub,
                ifelse(
                  rc,
                  "(inclusive)",
                  "(exclusive)"
                )
              ) -> msg
          }

          paste0(msg, ".") -> msg

          return(msg)
        },
        ...
      )
  )
}

# endregion
# numeric matrix types
# region: unit matrix
validate.unit.matrix <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.unit.matrix,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "a numeric matrix in the unit interval."
        },
        ...
      )
  )
}

# endregion
# region: signed unit matrix
validate.unit.signed.matrix <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.unit.signed.matrix,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "a numeric matrix with absolute value in the unit interval."
        },
        ...
      )
  )
}

# endregion
# region: bernoulli matrix
validate.bernoulli.matrix <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.bernoulli.matrix,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "a numeric matrix where all values are either 0 or 1."
        },
        ...
      )
  )
}

# endregion
# region: matrix-like
validate.matrix.like <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.matrix.like,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "a data frame or matrix."
        },
        ...
      )
  )
}

# endregion
# region: square matrix
validate.square <- function(x, arg.name = NULL, nullable = F, ...) {
  return(
    x |>
      utils$validate(
        type = types$is.square,
        nullable = nullable,
        arg.name = arg.name,
        type.def = function() {
          "have the same number of rows and columns."
        },
        ...
      )
  )
}

# endregion
# misc asserts
# region: assert methods
validate.method <- function(x, arg.name = NULL, methods, ...) {
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
  validate.bool,
  # numeric types
  validate.numeric,
  validate.unit,
  validate.unit.signed,
  validate.bernoulli,
  validate.numeric.bounded,
  # numeric matrix types
  validate.unit.matrix,
  validate.unit.signed.matrix,
  validate.bernoulli.matrix,
  validate.matrix.like,
  validate.square,
  # misc
  validate.method
)

# endregion
