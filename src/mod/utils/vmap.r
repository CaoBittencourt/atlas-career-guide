# vectorized map function
# implement:
# 1. pass dynamic dots to function
# 2. pass any number of vectors / matrices
vmap <- function(vm, mv, fn, ...) {
  # list(...) -> args

  vm |>
    as.matrix() |>
    as.data.frame() -> vm
  mv |>
    as.matrix() |>
    as.data.frame() -> mv

  expand.grid(
    k = 1:ncol(vm),
    q = 1:ncol(mv)
  ) -> kq


  return(
    fn |>
      mapply(
        vm[kq$k],
        mv[kq$q]
      ) |>
      matrix(
        nrow = ncol(vm),
        ncol = ncol(mv),
        dimnames = list(
          names(vm),
          names(mv)
        )
      )
  )
}

# function(fn, args, ...){
#   list(...) -> vmv
  
#   vmv |> lapply(function(x){
#     x |> as.matrix() |> as.data.frame()
#   }) -> vmv
  
#   vmv |> lapply(ncol) -> 

# }