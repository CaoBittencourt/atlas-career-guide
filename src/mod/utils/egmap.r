# region: imports
box::use(
  mod / utils / depth[...],
  mod / utils / sublist[...],
  mod / utils / nestmap[...],
  mod / utils / proper_list[...],
  mod / utils / name[...],
  stats[setNames],
  purrr[list_flatten, map_if],
  tidyr[as_tibble],
  mgsub[mgsub]
)

# endregion
# region: expand.grid map
egmap <- function(iters, fn, ...) {
  # assert args
  stopifnot(
    "'iters' must be a list of arguments over which to iterate with 'fn'." =
      is.proper.list(iters)
  )

  # stopifnot(
  #   "'iters' must not be nested more than two levels deep." = all(
  #     iters |> sapply(depth) <= 2
  #   )
  # )

  stopifnot(
    "'fn' must be a function." = is.function(fn)
  )

  # convert everything to dataframes for iteration
  iters |> name() -> iters
  Map(name, iters, names(iters)) -> iters
  iters |> lapply(nestmap, as.data.frame) -> iters

  # get reps for parallel args
  iters |>
    map_if(
      is.proper.list,
      length,
      .else = ~1
    ) ->
  reps

  # expand grid
  iters |>
    map_if(
      is.proper.list,
      ~ .x[[1]] |> ncol(),
      .else = ncol
    ) |>
    lapply(seq_len) |>
    expand.grid() ->
  eg

  eg[
    rep |>
      mapply(
        1:ncol(eg),
        reps
      ) |>
      unlist()
  ] -> eg

  # iter names
  mapply(
    function(name, nested.name, is.nested) {
      if (is.nested) {
        return(nested.name)
      }

      return(name)
    },
    is.nested = reps > 1,
    name = iters |> names(),
    nested.name = iters |> nestmap(names)
  ) |>
    unlist() |>
    unname() ->
  iters.names

  # call function over expand grid
  list_flatten(iters) -> iters

  eg |>
    t() |>
    as.data.frame() |>
    sapply(
      function(i) {
        do.call(
          fn,
          c(
            Map(
              function(data, i) {
                data[[i]]
              },
              i = i,
              data = iters
            ),
            ...
          )
        )
      }
    ) -> value

  mgsub |>
    mapply(
      eg,
      eg |> lapply(unique),
      iters |>
        lapply(nestmap, colnames) |>
        list_flatten()
    ) |>
    as.data.frame() |>
    setNames(iters.names) ->
  eg

  value |> unname() -> eg$value

  # expand grid with values
  return(eg |> as_tibble())
}

# endregion
# region: exports
box::export(egmap)

# endregion
