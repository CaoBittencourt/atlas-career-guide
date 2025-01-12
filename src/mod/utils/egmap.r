modular::project.options("atlas")
# region: imports
box::use(
  mod / utils / depth[...],
  mod / utils / sublist[...]
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

  iters |>
    sublist(function(l) {
      all(is.list(l), !is.data.frame(l))
    }) ->
  iters.parallel

  iters |>
    sublist(function(l) {
      !all(is.list(l), !is.data.frame(l))
    }) ->
  iters

  iters |> lapply(as.data.frame) -> iters
  iters.parallel |> lapply(lapply, as.data.frame) -> iters.parallel

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
    eg |>
      t() |>
      as.data.frame() |>
      lapply(
        function(i) {
          do.call(
            fn,
            args = c(
              iters, c(iters.parallel)
            ) |> lapply(function(data) {
              data[[i]]
            }) |> c(args)
          )
        }
      )
  )
}

mapply(
  fn,
  vm[kq$k],
  mv[kq$q]
)

list(
  skill_set = data.frame(person1 = rep(19, 120), person2 = rep(19, 120)),
  occupations = list(
    skill_mtx = matrix(19, 120, 4),
    aeq_mtx = matrix(1, 120, 4)
  ),
  # methods = rbind(c(paste0('method', 1:5)))
  match_method = list(as.list(paste0("method", 1:5))),
  dsds = "dsds"
) |> egmap(matching)
# mapply(
#   function(data, comb){
#     fn()
#   },
#   data = c(iters, iters.parallel),
#   comb = eg |> t() |> as.data.frame()
# )

c(
  iters,
  iters.parallel
)


list(
  a = c("19", "19"),
  b = list(data.frame(cbind(19, 19)), data.frame(cbind(19, 19))),
  c = list(data.frame(19, 19, 19), data.frame(19, 19, 19)),
  d = "19",
  e = data.frame(cbind(19, 19, 19))
) |> egmap()

expand.grid(
  c(
    dsdsds$iters,
    dsdsds$iters.parallel
  )
)
expand.grid(
  list(),
  list(dsdsds$iters.parallel)
)

dsds |> sapply(ncol)

iters.parallel <- iters[iters |> sapply(function(i) {
  all(is.list(i), !is.data.frame(i))
})]
iters.parallel |> lapply(function(l) {

})
list(19, list(19, 19), list(19, 19), data.frame(19))[list(19, list(19, 19), list(19, 19), data.frame(19)) |> sapply(function(i) {
  all(is.list(i), !is.data.frame(i))
})]
library(dplyr)
rep(19, 19) |> bind_cols()
rep(19, 19) |> as.data.frame()
cbind(rep(19, 19), rep(19, 19)) |> as.data.frame()
rep(19, 19) |> as_tibble()


# # expand.grid map
# egmap <- function(fn = NULL, ..., args = NULL) {
#   list(...) |>
#     lapply(as.matrix) |>
#     lapply(as.data.frame) ->
#   vmv

#   vmv |>
#     lapply(function(x) {
#       1:ncol(x)
#     }) |>
#     expand.grid() ->
#   eg

#   return(list(
#     vmv,
#     eg
#     # fn |> Map(eg),
#     , Map(
#       function(df, id) {
#         df[,id]
#       },
#       df = vmv,
#       id = eg
#     )
#     # fn |>
#     #   mapply(
#     #     Map(
#     #       function(df, id) {
#     #         df[, id]
#     #       },
#     #       df = vmv,
#     #       id = eg
#     #     )
#     #   )
#     # fn |> do.call(
#     #   args = list(mapply(
#     #     Map(
#     #       function(df, id) {
#     #         df[id]
#     #       },
#     #       df = vmv,
#     #       id = eg
#     #     )
#     #   ))
#     # )
#     # fn |>
#     #   mapply(
#     #     Map(
#     #       function(df, id) {
#     #         df[id]
#     #       },
#     #       df = vmv,
#     #       id = eg
#     #     ) |> c()
#     #     , MoreArgs = args
#     #     , SIMPLIFY = F
#     #   )
#   ))
#   # -> eg$val

#   # return(vmv)
# }

# # egmap(
# #   fn = function(x1, x2, x3) {
# #     return(x1)
# #   }
# #   # , args = NULL
# #   , a = dsds[1], b = dsds[1:2], c = dsds[1:3]
# # ) -> dsdsds

# egmap(
#   fn = cosine.similarity,
#   ak = dsds[1:2],
#   aq = dsds[1:3],
#   args = list(
#     "dsds" = "lalala"
#   )
# ) -> dsdsds

# lapply(list, function)

# mapply(sum, c(dsdsds[[3]]))
# dsdsds[[3]] |> lapply(nrow)

# do.call(mapply, c(list(FUN = cosine.similarity, MoreArgs = NULL, dsdsds[[2]])))
# mapply(cosine.similarity, unname(dsdsds[[2]]))

# dsdsds[[2]][[1]] |> names()
# dsdsds[1]
# dsdsds[[2]][[2]] |> names()

# dsdsds[1]
# dsdsds[2]
# dsdsds[3]

# do.call(dsdsds[[2]])

# # -> dsdsds

# # mapply(names, dsdsds)

# # !!!unlist(args)
# list(a = dsds[1], b = dsds[1:2], c = dsds[1:3]) -> dsdsdsds
# dsdsds[[1]] |> apply(1, function(x) {
#   x
# })


# mapply(fn, dsdsdsds$a[kq[a]], dsdsdsds$b[kq[b]], dsdsdsds$c[kq[c]])

# dsdsdsds$a[[1]]
# dsdsdsds$b[[1]]
# dsdsdsds$c[[1]]

# dsdsdsds$a[[1]]
# dsdsdsds$b[[2]]
# dsdsdsds$c[[1]]

# dsdsdsds$a[[1]]
# dsdsdsds$b[[1]]
# dsdsdsds$c[[2]]
