modular::project.options("atlas")
# region: imports
box::use(
  mod / utils / depth[...],
  mod / utils / sublist[...],
  mod / utils / nestmap[...],
  mod / utils / proper_list[...],
  purrr[list_flatten]
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

# mapply(
#   fn,
#   vm[kq$k],
#   mv[kq$q]
# )

list(
  skill_set = data.frame(person1 = c(1,rep(19, 119)), person2 = c(9, rep(19, 119))),
  occupations = list(
    skill_mtx = matrix(19, 120, 4),
    aeq_mtx = matrix(1, 120, 4)
  )
  # ,
  # # methods = rbind(c(paste0('method', 1:5)))
  # match_method = list(as.list(paste0("method", 1:5))),
  # dsds = rbind(c("dsds", "lalala"))
  # # dsds = list(as.list(c("dsds", "lalala")))
) -> dsds

dsds |> lapply(nestmap, as.data.frame) -> iters

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

list_flatten(iters) -> iters

eg |>
  t() |>
  as.data.frame() |>
  lapply(
    function(i) {
      do.call(
        sum,
        Map(
          function(data, i) {
            data[, i]
          },
          i = i,
          data = iters
        )
          # |> c(...)
      )
    }
  )
