# probability of finding a job
prob.find <- function(w = NULL) {
  return(
    ifelse(
      is.null(w), 1, w / sum(w)
    )
  )
}

# probability of landing a job
prob.land <- function(skq) {
  return(skq * (skq >= 0.5))
}

# probability of a job
prob <- function(skq, w = NULL) {
  return(prob.find(w) * prob.land(skq))
}

box::export(prob)
