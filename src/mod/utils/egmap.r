modular::project.options("atlas")
# region: imports
# install.packages('mgsub')
box::use(
  mod / utils / depth[...],
  mod / utils / sublist[...],
  mod / utils / nestmap[...],
  mod / utils / proper_list[...],
  purrr[list_flatten, map_if],
  mgsub[mgsub]
)
# endregion
# region: expand.grid map

# endregion
# region: exports
# box::export()

# endregion
# expand.grid map
egmap <- function(iters, fn, args = NULL) {
  # assert args
  stopifnot(
    "'iters' must not be nested more than two levels deep." = all(
      iters |> sapply(depth) <= 2
    )
  )

  # parallel iterables must have the same number of columns each
  iters |> lapply(nestmap, as.data.frame) -> iters
  # iters |> sublist(is.proper.list) -> iters.parallel
  # iters |> sublist(is.proper.list, negate = T) -> iters

  # iters |> lapply(as.data.frame) -> iters
  # iters.parallel |> lapply(lapply, as.data.frame) -> iters.parallel

  iters |>
    lapply(
      function(i) {
        1:ncol(i)
      }
    ) |>
    c(
      iters.parallel |> lapply(
        function(i) {
          1:ncol(i[[1]])
        }
      )
    ) |>
    expand.grid() ->
  eg

  return(
    eg
    # eg |>
    #   t() |>
    #   as.data.frame() |>
    #   lapply(
    #     function(i) {
    #       do.call(
    #         fn,
    #         args = c(
    #           iters, c(iters.parallel)
    #         ) |> lapply(function(data) {
    #           data[[i]]
    #         }) |> c(args)
    #       )
    #     }
    #   )
  )
}

getOption("atlas.skills_mtx") |> readRDS() -> dsds
dsds[-1] -> dsds

skill_set <- dsds[1:2]
skill_mtx <- dsds[1:4]
weights_mtx <- dsds[1:4]

box::use(stats[weighted.mean])
fn <- function(ak, aq, äq, method) {
  if (method == "method1") {
    print("method1")
    return(weighted.mean(ak, aq, äq))
  }

  if (method == "method2") {
    print("method2")
    return(max(äq * ak) - max(äq * aq))
  }

  print("no such method")
  return(NA)
}

list(
  individuals = skill_set,
  occupations = list(
    skill_mtx = skill_mtx,
    weights_mtx = weights_mtx
  ),
  method = rbind(paste0("method", 1:2) |> setNames(paste0("method", 1:2)))
) -> dsds

dsds |> lapply(nestmap, as.data.frame) -> iters

iters |> lapply(nestmap, colnames) -> lalala

# lalala |> list() |> nestmap(names) |> list_flatten()

# lalala |> names() -> names.iters
# lalala |> nestmap(names) -> names.parallel
# names(lalala)[]
# lalala[lalala |> sapply(is.null)] <- names(lalala)

# lalala |> names()
# lalala |> map_if(is.proper.list, colnames, names)

# lalala |> lapply(
#   function(i){
#     if(is.proper.list(i)){

#     }
#   }
# )
# lalala |> names() |> length()
# lalala |> nestmap(names) |> length()
# # lalala |> lapply(is.proper.list)
# lalala |> names()

iters |>
  sapply(nestmap, ncol) |>
  sapply(function(i) {
    i[[1]]
  }) |>
  lapply(nestmap, seq_len) |>
  expand.grid() ->
eg

mapply(
  function(i, reps) {
    eg[rep(i, reps)]
  },
  i = eg |> ncol() |> seq_len(),
  reps = iters |>
    sapply(
      function(i) {
        if (is.proper.list(i)) {
          return(length(i))
        }
        return(1)
      }
    )
) |>
  as.data.frame() ->
eg

# iters |> names()
# iters |> lapply(names)
list_flatten(iters) -> iters
eg
iters
lalala

eg |>
  t() |>
  as.data.frame() |>
  lapply(
    function(i) {
      do.call(
        fn,
        Map(
          function( # name,
                   data, i) {
            # data[, i]
            data[i]
          },
          # name = colnames(iters),
          i = i,
          data = iters
        )
        # |> c(...)
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
  as.data.frame() ->
eg

eg$value <- value
