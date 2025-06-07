library(R6)

list(
  num = is.numeric,
  int = is.integer,
  dbl = is.double,
  chr = is.character,
  null = is.null
) -> is.methods

R6::R6Class(
  classname = "is",
  class = T,
  public =
    is.methods |> c(
      initialize = function(x) {
        self <- !is.methods$null(x)
      }
    ),
  cloneable = F,
  lock_objects = T,
  lock_class = T
) -> is

is |> class()

is$new() -> dsds

is.methods |> as.environment() -> env
is.methods |> list2env() -> env
local(env$null)()

env |> rlang::is_callable()

dsds

dsds$initialize("dsds")

NULL |> dsds$null()
"dsds" |> dsds$null()
NULL |> dsds$chr()
"dsds" |> dsds$chr()
NULL |> dsds$dbl()
"dsds" |> dsds$dbl()
19L |> dsds$dbl()
19.19 |> dsds$dbl()
19L |> dsds$int()
19.19 |> dsds$int()
19L |> dsds$num()
19.19 |> dsds$num()
