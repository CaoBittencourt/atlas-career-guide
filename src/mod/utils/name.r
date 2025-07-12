box::use(data.table[fifelse], stats[setNames])

name <- function(x, if.empty = "x") {
  names(x) -> x.names
  if.empty |> paste0(seq_along(x)) -> empty.names

  if (!length(x.names)) {
    # x |> setNames(empty.names) -> x
    return(x |> setNames(empty.names))
  }

  return(
    x |>
      setNames(
        # ifelse(
        fifelse(
          x.names == "",
          empty.names,
          x.names
        )
      )
  )
}
