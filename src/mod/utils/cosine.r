cosine.similarity <- function(ak, aq) {
  return(sum(ak * aq) / sqrt(sum(ak^2) * sum(aq^2)))
}


crossprod(ak, aq)
crossprod(ak, aq) |>
  diag() |>
  sqrt()



vector or matrix
matrix or vector

(vector or matrix) |> as.matrix() |> as.data.frame() -> k
(matrix or vector) |> as.matrix() -> q

dsds |> list() |> rep(nco)
vmap <- function(vm, mv, fn, ...){
  # list(...) -> args
  vm |> as.matrix() |> as.data.frame() -> vm
  mv |> as.matrix() |> as.data.frame() -> mv
  mapply(function(k,q){mapply(fn, k, q, !!!list(...))}, k = mv, q = vm, !!!list(...))
  return(mapply(function(k,q){fn(k,q)}, k = vm, q = mv |> as.matrix() |> list() |> rep(ncol(vm)), !!!list(...)))

}

dsds[[1]] |> vmap(dsds[-1], cosine.similarity)

# mapply(function, k = k, q = ncol(k) |> replicate(expr = q, simplify = F), SIMPLIFY = F)


cosine.similarity(dsds[[1]],dsds[[1]])
dsds[[1]] |> as.matrix() |> as.data.frame()
dsds[1:2] |> as.matrix() |> as.data.frame()

  mapply(
    cosine.similarity, 
  k = dsds[1:2] |> as.matrix() |> as.data.frame() |> lapply(list) |> lapply(rep, 873),
  q = dsds |> as.matrix() |> as.data.frame() |> list() |> rep(2),
  ...
  )

  
  
outer(dsds[1:2] |> as.matrix(), dsds |> as.matrix(), cosine.similarity)
vm <- dsds[1:2]
mv <- dsds

expand.grid(k = 1:ncol(vm), q = 1:ncol(mv)) -> kq

vmap <- function(vm, mv, fn, ...){
  vm |> as.matrix() |> as.data.frame() -> vm
  mv |> as.matrix() |> as.data.frame() -> mv

  expand.grid(
    k = 1:ncol(vm),
    q = 1:ncol(mv)
  ) -> kq

  kq$val <- mapply(fn, vm[kq$k], mv[kq$q])

  return(
    matrix(
          kq$val,
          nrow = ncol(vm),
          ncol = ncol(mv),
          dimnames = list(
            names(vm), 
            names(mv) 
          )
        )
  )
}

vmap(dsds, dsds, cosine.similarity) |> View()

dsds[1:2] |> as.matrix() |> as.data.frame() -> vm
dsds |> as.matrix() |> as.data.frame() -> mv

expand.grid(
  k = 1:ncol(vm),
  q = 1:ncol(mv)
) -> kq

kq$val <- mapply(cosine.similarity, vm[kq$k], mv[kq$q])
# vm |> names()
# mv |> names()
matrix(
  kq$val,
  nrow = max(kq$k),
  ncol = max(kq$q)
) -> val

all(kq |> filter(k == 1) |> select(val) == val[1,])
all(kq |> filter(k == 2) |> select(val) == val[2,])

mapply(cosine.similarity, vm)

# expand.grid(k = vm |> as.matrix(), q = mv |> as.matrix()) -> kq
# kq$cos <- mapply(function(k,q){do.call(cosine.similarity, list(dsds[[k]], dsds[[q]]))}, k = kq[[1]], q = kq[[2]])

.mapply()
simplify2array()
do.call(
  cosine.similarity, 
  list(
    dsds[[kq[1,1]]],
    dsds[[kq[1,2]]]
  )
)

do.call(
  mapply,
  c(
    cosine.similarity,
    expand.grid(
      k = 1:(dsds[1:2] |> as.matrix() |> ncol()),
      q = 1:(dsds |> as.matrix() |> ncol())
    ) |> unname()
  )
)
kq 


mapply(function(k,q){cosine.similarity(k,q)}, k = kq[[1]], q = kq[[2]])
