# expand.grid map
egmap <- function(fn = NULL, ..., args = NULL) {
  list(...) |>
    lapply(as.matrix) |>
    lapply(as.data.frame) ->
  vmv

  vmv |>
    lapply(function(x) {
      1:ncol(x)
    }) |>
    expand.grid() ->
  eg

  return(list(
    vmv,
    eg
    # fn |> Map(eg),
    , Map(
      function(df, id) {
        df[,id]
      },
      df = vmv,
      id = eg
    )
    # fn |>
    #   mapply(
    #     Map(
    #       function(df, id) {
    #         df[, id]
    #       },
    #       df = vmv,
    #       id = eg
    #     )
    #   )
    # fn |> do.call(
    #   args = list(mapply(
    #     Map(
    #       function(df, id) {
    #         df[id]
    #       },
    #       df = vmv,
    #       id = eg
    #     )
    #   ))
    # )
    # fn |>
    #   mapply(
    #     Map(
    #       function(df, id) {
    #         df[id]
    #       },
    #       df = vmv,
    #       id = eg
    #     ) |> c()
    #     , MoreArgs = args
    #     , SIMPLIFY = F
    #   )
  ))
  # -> eg$val

  # return(vmv)
}

# egmap(
#   fn = function(x1, x2, x3) {
#     return(x1)
#   }
#   # , args = NULL
#   , a = dsds[1], b = dsds[1:2], c = dsds[1:3]
# ) -> dsdsds

egmap(
  fn = cosine.similarity,
  ak = dsds[1:2],
  aq = dsds[1:3],
  args = list(
    "dsds" = "lalala"
  )
) -> dsdsds

lapply(list, function)

mapply(sum, c(dsdsds[[3]]))
dsdsds[[3]] |> lapply(nrow)

do.call(mapply, c(list(FUN = cosine.similarity, MoreArgs = NULL, dsdsds[[2]])))
mapply(cosine.similarity, unname(dsdsds[[2]]))

dsdsds[[2]][[1]] |> names()
dsdsds[1]
dsdsds[[2]][[2]] |> names()

dsdsds[1]
dsdsds[2]
dsdsds[3]

do.call(dsdsds[[2]])

# -> dsdsds

# mapply(names, dsdsds)

# !!!unlist(args)
list(a = dsds[1], b = dsds[1:2], c = dsds[1:3]) -> dsdsdsds
dsdsds[[1]] |> apply(1, function(x) {
  x
})


mapply(fn, dsdsdsds$a[kq[a]], dsdsdsds$b[kq[b]], dsdsdsds$c[kq[c]])

dsdsdsds$a[[1]]
dsdsdsds$b[[1]]
dsdsdsds$c[[1]]

dsdsdsds$a[[1]]
dsdsdsds$b[[2]]
dsdsdsds$c[[1]]

dsdsdsds$a[[1]]
dsdsdsds$b[[1]]
dsdsdsds$c[[2]]
