# region: imports
box::use()

# endregion
# region: method 1 name
function2.method1 <- function() {
  # assert args in main function
  # description
  return("method1")
}

# endregion
# region: method 2 name
function2.method2 <- function() {
  # assert args in main function
  # description
  return("method2")
}

# endregion
# region: list of methods
list(
  "method1",
  "method2"
) -> function2.methods

# endregion
# region: generic function
function2 <- function(function2_method = function2.methods[[1]], ...) {
  # assert args
  stopifnot(
    "'function2_method' must be one of the following methods: 'method1', 'method2'." = any(
      function2_method == function2.methods
    )
  )

  # multiple dispatch
  function2_method[[1]] |>
    as.character() |>
    switch(
      "method1" = return(function2.method1()),
      "method2" = return(function2.method2())
    )
}

# endregion
# region: exports
box::export(function2, function2.methods)

# endregion
