modular::project.options('atlas')

box::use(
  mod / describe / gene[...]
)

ae <- function(ss, gn = NULL){
  
  if(is.null(gn)){
    gene(ss) -> gn
  }
  return(ss ^ gn)
}

cp <- function(ss, ...){
  list(...) -> args
  # return(c(list(ss = ss), args))
  do.call(
    ae, args = c(list(ss = ss), args)
  )
  # return(ae(c(list(ss = ss), args)))
}

runif(120) -> dsds

cp(dsds)
cp(dsds, gn = gene(dsds))
cp(dsds) == cp(dsds, gn = gene(dsds))
