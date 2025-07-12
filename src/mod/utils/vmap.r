# vectorized map function
vmap <- function(vm, mv, fn, ...) {
  vm |> as.data.frame() -> vm
  mv |> as.data.frame() -> mv

  expand.grid(
    k = 1:ncol(vm),
    q = 1:ncol(mv)
  ) -> kq

  return(
    fn |>
      mapply(
        vm[kq$k],
        mv[kq$q],
        ...
      ) |>
      matrix(
        nrow = ncol(vm),
        ncol = ncol(mv),
        dimnames = list(
          names(vm),
          names(mv)
        )
      ) |>
      t()
  )
}
