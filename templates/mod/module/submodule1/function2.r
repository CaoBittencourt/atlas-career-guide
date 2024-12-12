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
# region: generic function
function2 <- function(function2_method = c("method1", "method2")[[1]], ...) {
  # assert args
  stopifnot(
    "'function2_method' must be one of the following methods: 'method1', 'method2'." = any(
      function2_method == c("method1", "method2")
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
box::export(function2)

# endregion
